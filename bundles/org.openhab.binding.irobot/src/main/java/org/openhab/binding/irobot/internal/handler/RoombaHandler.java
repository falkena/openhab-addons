/**
 * Copyright (c) 2010-2021 Contributors to the openHAB project
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0
 *
 * SPDX-License-Identifier: EPL-2.0
 */
package org.openhab.binding.irobot.internal.handler;

import static org.openhab.binding.irobot.internal.IRobotBindingConstants.*;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.UNKNOWN;
import static org.openhab.core.thing.ThingStatus.INITIALIZING;
import static org.openhab.core.thing.ThingStatus.OFFLINE;
import static org.openhab.core.thing.ThingStatus.UNINITIALIZED;

import java.io.IOException;
import java.io.StringReader;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.Hashtable;
import java.util.List;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.irobot.internal.config.IRobotConfiguration;
import org.openhab.binding.irobot.internal.dto.MQTTProtocol;
import org.openhab.binding.irobot.internal.utils.LoginRequester;
import org.openhab.core.io.transport.mqtt.MqttConnectionState;
import org.openhab.core.library.types.DecimalType;
import org.openhab.core.library.types.OnOffType;
import org.openhab.core.library.types.StringType;
import org.openhab.core.thing.ChannelGroupUID;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusDetail;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.BaseThingHandler;
import org.openhab.core.types.Command;
import org.openhab.core.types.RefreshType;
import org.openhab.core.types.State;
import org.openhab.core.types.UnDefType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;
import com.google.gson.JsonParseException;
import com.google.gson.stream.JsonReader;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.jayway.jsonpath.ParseContext;
import com.jayway.jsonpath.TypeRef;
import com.jayway.jsonpath.spi.json.GsonJsonProvider;
import com.jayway.jsonpath.spi.mapper.GsonMappingProvider;

/**
 * The {@link RoombaHandler} is responsible for handling commands, which are
 * sent to one of the channels.
 *
 * @author hkuhn42 - Initial contribution
 * @author Pavel Fedin - Rewrite for 900 series
 * @author Florian Binder - added cleanRegions command and lastCommand channel
 * @author Alexander Falkenstern - Add support for I7 series
 */
@NonNullByDefault
public class RoombaHandler extends BaseThingHandler {
    private final Logger logger = LoggerFactory.getLogger(RoombaHandler.class);

    private final Gson gson = new Gson();
    private ParseContext jsonParser;

    private Hashtable<ChannelUID, State> lastState = new Hashtable<>();
    private MQTTProtocol.@Nullable Schedule lastSchedule = null;
    private boolean autoPasses = true;
    private @Nullable Boolean twoPasses = null;
    private boolean carpetBoost = true;
    private @Nullable Boolean vacHigh = null;
    private boolean isPaused = false;

    private @Nullable Future<?> credentialRequester;
    protected IRobotConnectionHandler connection = new IRobotConnectionHandler() {
        @Override
        public void receive(final String topic, final String json) {
            // Skip desired messages, since AWS-related stuff
            final DocumentContext document = jsonParser.parse(json);
            if (document.read("$..desired", new TypeRef<List<String>>() {
            }).isEmpty()) {
                RoombaHandler.this.receive(topic, json);
            }
        }

        @Override
        public void connectionStateChanged(MqttConnectionState state, @Nullable Throwable error) {
            super.connectionStateChanged(state, error);
            if (state == MqttConnectionState.CONNECTED) {
                updateStatus(ThingStatus.ONLINE);
            } else {
                String message = (error != null) ? error.getMessage() : "Unknown reason";
                updateStatus(OFFLINE, ThingStatusDetail.COMMUNICATION_ERROR, message);
            }
        }
    };

    public RoombaHandler(Thing thing) {
        super(thing);
        Configuration.ConfigurationBuilder builder = Configuration.builder();
        builder = builder.jsonProvider(new GsonJsonProvider());
        builder = builder.mappingProvider(new GsonMappingProvider());
        builder = builder.options(Option.ALWAYS_RETURN_LIST, Option.SUPPRESS_EXCEPTIONS);
        jsonParser = JsonPath.using(builder.build());
    }

    @Override
    public void initialize() {
        IRobotConfiguration config = getConfigAs(IRobotConfiguration.class);

        try {
            InetAddress.getByName(config.getAddress());
        } catch (UnknownHostException exception) {
            final String message = "Error connecting to host " + exception.toString();
            updateStatus(OFFLINE, ThingStatusDetail.COMMUNICATION_ERROR, message);
            return;
        }

        if (UNKNOWN.equals(config.getPassword()) || UNKNOWN.equals(config.getBlid())) {
            final String message = "Robot authentication is required";
            updateStatus(OFFLINE, ThingStatusDetail.CONFIGURATION_ERROR, message);
            scheduler.execute(this::getCredentials);
        } else {
            scheduler.execute(this::connect);
        }
    }

    @Override
    public void dispose() {
        Future<?> requester = credentialRequester;
        if (requester != null) {
            requester.cancel(false);
            credentialRequester = null;
        }

        scheduler.execute(connection::disconnect);
    }

    @Override
    public void handleCommand(ChannelUID channelUID, Command command) {
        if (command instanceof RefreshType) {
            State value = lastState.get(channelUID);
            updateState(channelUID, value != null ? value : UnDefType.UNDEF);
            return;
        }

        final String channelId = channelUID.getIdWithoutGroup();
        if (CHANNEL_CONTROL_COMMAND.equals(channelId)) {
            if (command instanceof StringType) {
                String cmd = command.toString();

                if (cmd.equals(COMMAND_CLEAN)) {
                    cmd = isPaused ? "resume" : "start";
                }

                if (cmd.startsWith(COMMAND_CLEAN_REGIONS)) {
                    // format: cleanRegions:<pmid>;<region_id1>,<region_id2>,...
                    if (Pattern.matches("cleanRegions:[^:;,]+;.+(,[^:;,]+)*", cmd)) {
                        String[] cmds = cmd.split(":");
                        String[] params = cmds[1].split(";");

                        String mapId = params[0];
                        String userPmapvId;
                        if (params.length >= 3) {
                            userPmapvId = params[2];
                        } else {
                            userPmapvId = null;
                        }

                        String[] regions = params[1].split(",");
                        String regionIds[] = new String[regions.length];
                        String regionTypes[] = new String[regions.length];

                        for (int i = 0; i < regions.length; i++) {
                            String[] regionDetails = regions[i].split("=");

                            if (regionDetails.length >= 2) {
                                if (regionDetails[0].equals("r")) {
                                    regionIds[i] = regionDetails[1];
                                    regionTypes[i] = "rid";
                                } else if (regionDetails[0].equals("z")) {
                                    regionIds[i] = regionDetails[1];
                                    regionTypes[i] = "zid";
                                } else {
                                    regionIds[i] = regionDetails[0];
                                    regionTypes[i] = "rid";
                                }
                            } else {
                                regionIds[i] = regionDetails[0];
                                regionTypes[i] = "rid";
                            }
                        }
                        MQTTProtocol.Request request = new MQTTProtocol.CleanRoomsRequest("start", mapId, regionIds,
                                regionTypes, userPmapvId);
                        connection.send(request.getTopic(), gson.toJson(request));
                    } else {
                        logger.warn("Invalid request: {}", cmd);
                        logger.warn("Correct format: cleanRegions:<pmid>;<region_id1>,<region_id2>,...>");
                    }
                } else {
                    MQTTProtocol.Request request = new MQTTProtocol.CommandRequest(cmd);
                    connection.send(request.getTopic(), gson.toJson(request));
                }
            }
        } else if (SCHEDULE_GROUP_ID.equals(channelUID.getGroupId())) {
            MQTTProtocol.Schedule schedule = lastSchedule;

            // Schedule can only be updated in a bulk, so we have to store current
            // schedule and modify components.
            if ((command instanceof OnOffType) && (schedule != null) && (schedule.cycle != null)) {
                MQTTProtocol.Schedule newSchedule = new MQTTProtocol.Schedule(schedule.cycle);
                for (int i = 0; i < DAY_OF_WEEK.length; i++) {
                    newSchedule.enableCycle(i, newSchedule.cycleEnabled(i));
                    if (channelUID.getIdWithoutGroup().equals(DAY_OF_WEEK[i] + "_enabled")) {
                        newSchedule.enableCycle(i, command.equals(OnOffType.ON));
                    }
                }
                sendSchedule(newSchedule);
            }
        } else if (CHANNEL_CONTROL_EDGE_CLEAN.equals(channelId)) {
            if (command instanceof OnOffType) {
                sendDelta(new MQTTProtocol.OpenOnly(command.equals(OnOffType.OFF)));
            }
        } else if (CHANNEL_CONTROL_ALWAYS_FINISH.equals(channelId)) {
            if (command instanceof OnOffType) {
                sendDelta(new MQTTProtocol.BinPause(command.equals(OnOffType.OFF)));
            }
        } else if (CHANNEL_CONTROL_POWER_BOOST.equals(channelId)) {
            sendDelta(new MQTTProtocol.PowerBoost(command.equals(BOOST_AUTO), command.equals(BOOST_PERFORMANCE)));
        } else if (CHANNEL_CONTROL_CLEAN_PASSES.equals(channelId)) {
            sendDelta(new MQTTProtocol.CleanPasses(!command.equals(PASSES_AUTO), command.equals(PASSES_2)));
        } else if (CHANNEL_CONTROL_MAP_UPLOAD.equals(channelId)) {
            if (command instanceof OnOffType) {
                sendDelta(new MQTTProtocol.MapUploadAllowed(command.equals(OnOffType.ON)));
            }
        }
    }

    private void sendSchedule(MQTTProtocol.Schedule schedule) {
        sendDelta(new MQTTProtocol.CleanSchedule(schedule));
    }

    private void sendDelta(MQTTProtocol.StateValue state) {
        MQTTProtocol.Request request = new MQTTProtocol.DeltaRequest(state);
        connection.send(request.getTopic(), gson.toJson(request));
    }

    private synchronized void getCredentials() {
        ThingStatus status = thing.getStatusInfo().getStatus();
        IRobotConfiguration config = getConfigAs(IRobotConfiguration.class);
        if (UNINITIALIZED.equals(status) || INITIALIZING.equals(status) || OFFLINE.equals(status)) {
            if (UNKNOWN.equals(config.getBlid())) {
                @Nullable
                String blid = null;
                try {
                    blid = LoginRequester.getBlid(config.getAddress());
                } catch (IOException exception) {
                    updateStatus(OFFLINE, ThingStatusDetail.CONFIGURATION_ERROR, exception.toString());
                }

                if (blid != null) {
                    org.openhab.core.config.core.Configuration configuration = editConfiguration();
                    configuration.put(ROBOT_BLID, blid);
                    updateConfiguration(configuration);
                }
            }

            if (UNKNOWN.equals(config.getPassword())) {
                @Nullable
                String password = null;
                try {
                    password = LoginRequester.getPassword(config.getAddress());
                } catch (KeyManagementException | NoSuchAlgorithmException exception) {
                    updateStatus(OFFLINE, ThingStatusDetail.COMMUNICATION_ERROR, exception.toString());
                    return; // This is internal system error, no retry
                } catch (IOException exception) {
                    updateStatus(OFFLINE, ThingStatusDetail.CONFIGURATION_ERROR, exception.toString());
                }

                if (password != null) {
                    org.openhab.core.config.core.Configuration configuration = editConfiguration();
                    configuration.put(ROBOT_PASSWORD, password.trim());
                    updateConfiguration(configuration);
                }
            }
        }

        credentialRequester = null;
        if (UNKNOWN.equals(config.getBlid()) || UNKNOWN.equals(config.getPassword())) {
            credentialRequester = scheduler.schedule(this::getCredentials, 10000, TimeUnit.MILLISECONDS);
        } else {
            scheduler.execute(this::connect);
        }
    }

    // In order not to mess up our connection state we need to make sure that connect()
    // and disconnect() are never running concurrently, so they are synchronized
    private synchronized void connect() {
        IRobotConfiguration config = getConfigAs(IRobotConfiguration.class);
        final String address = config.getAddress();
        logger.debug("Connecting to {}", address);

        final String blid = config.getBlid();
        final String password = config.getPassword();
        if (UNKNOWN.equals(blid) || UNKNOWN.equals(password)) {
            final String message = "Robot authentication is required";
            updateStatus(OFFLINE, ThingStatusDetail.CONFIGURATION_ERROR, message);
            scheduler.execute(this::getCredentials);
        } else {
            final String message = "Robot authentication is successful";
            updateStatus(ThingStatus.ONLINE, ThingStatusDetail.CONFIGURATION_PENDING, message);
            connection.connect(address, blid, password);
        }
    }

    public void receive(final String topic, final String json) {
        MQTTProtocol.StateMessage msg;

        logger.trace("Got topic {} data {}", topic, json);

        try {
            // We are not consuming all the fields, so we have to create the reader explicitly
            // If we use fromJson(String) or fromJson(java.util.reader), it will throw
            // "JSON not fully consumed" exception, because not all the reader's content has been
            // used up. We want to avoid that also for compatibility reasons because newer iRobot
            // versions may add fields.
            JsonReader jsonReader = new JsonReader(new StringReader(json));
            msg = gson.fromJson(jsonReader, MQTTProtocol.StateMessage.class);
        } catch (JsonParseException exception) {
            IRobotConfiguration config = getConfigAs(IRobotConfiguration.class);
            logger.warn("Failed to parse JSON message from {}: {}", config.getAddress(), exception.toString());
            logger.warn("Raw contents: {}", json);
            return;
        }

        // Since all the fields are in fact optional, and a single message never
        // contains all of them, we have to check presence of each individually
        if (msg.state == null || msg.state.reported == null) {
            return;
        }

        final ThingUID thingUID = thing.getUID();
        MQTTProtocol.GenericState reported = msg.state.reported;

        if (reported.cleanMissionStatus != null) {
            final ChannelGroupUID missionGroupUID = new ChannelGroupUID(thingUID, MISSION_GROUP_ID);

            String cycle = reported.cleanMissionStatus.cycle;
            String phase = reported.cleanMissionStatus.phase;
            String command;

            if (cycle.equals("none")) {
                command = COMMAND_STOP;
            } else {
                switch (phase) {
                    case "stop":
                    case "stuck": // CHECKME: could also be equivalent to "stop" command
                    case "pause": // Never observed in Roomba 930
                        command = COMMAND_PAUSE;
                        break;
                    case "hmUsrDock":
                    case "dock": // Never observed in Roomba 930
                        command = COMMAND_DOCK;
                        break;
                    default:
                        command = cycle; // "clean" or "spot"
                        break;
                }
            }

            isPaused = command.equals(COMMAND_PAUSE);

            reportState(new ChannelUID(missionGroupUID, CHANNEL_MISSION_CYCLE), StringType.valueOf(cycle));
            reportState(new ChannelUID(missionGroupUID, CHANNEL_MISSION_PHASE), StringType.valueOf(phase));
            reportState(new ChannelUID(missionGroupUID, CHANNEL_MISSION_ERROR),
                    StringType.valueOf(String.valueOf(reported.cleanMissionStatus.error)));

            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            reportState(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_COMMAND), StringType.valueOf(command));
        }

        if (reported.batPct != null) {
            final ChannelGroupUID stateGroupUID = new ChannelGroupUID(thingUID, STATE_GROUP_ID);
            reportState(new ChannelUID(stateGroupUID, CHANNEL_STATE_CHARGE), new DecimalType(reported.batPct));
        }

        if (reported.bin != null) {
            final ChannelGroupUID stateGroupUID = new ChannelGroupUID(thingUID, STATE_GROUP_ID);
            // The bin cannot be both full and removed simultaneously, so let's encode it as a single value
            if (!reported.bin.present) {
                reportState(new ChannelUID(stateGroupUID, CHANNEL_STATE_BIN), StringType.valueOf(STATE_BIN_REMOVED));
            } else if (reported.bin.full) {
                reportState(new ChannelUID(stateGroupUID, CHANNEL_STATE_BIN), StringType.valueOf(STATE_BIN_FULL));
            } else {
                reportState(new ChannelUID(stateGroupUID, CHANNEL_STATE_BIN), StringType.valueOf(STATE_BIN_OK));
            }
        }

        if (reported.signal != null) {
            final ChannelGroupUID networkGroupUID = new ChannelGroupUID(thingUID, NETWORK_GROUP_ID);
            reportState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_RSSI), new DecimalType(reported.signal.rssi));
            reportState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_SNR), new DecimalType(reported.signal.snr));
        }

        if (reported.cleanSchedule != null) {
            final ChannelGroupUID scheduleGroupUID = new ChannelGroupUID(thingUID, SCHEDULE_GROUP_ID);
            MQTTProtocol.Schedule schedule = reported.cleanSchedule;

            if (schedule.cycle != null) {
                for (int i = 0; i < DAY_OF_WEEK.length; i++) {
                    reportState(new ChannelUID(scheduleGroupUID, DAY_OF_WEEK[i] + "_enabled"),
                            OnOffType.from(schedule.cycleEnabled(i)));
                }
            }

            lastSchedule = schedule;
        }

        if (reported.openOnly != null) {
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            reportState(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_EDGE_CLEAN),
                    OnOffType.from(!reported.openOnly));
        }

        if (reported.binPause != null) {
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            reportState(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_ALWAYS_FINISH),
                    OnOffType.from(!reported.binPause));
        }

        // To make the life more interesting, paired values may not appear together in the
        // same message, so we have to keep track of current values.
        if (reported.carpetBoost != null) {
            carpetBoost = reported.carpetBoost;
            if (reported.carpetBoost) {
                // When set to true, overrides vacHigh
                final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
                reportState(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_POWER_BOOST),
                        StringType.valueOf(BOOST_AUTO));
            } else if (vacHigh != null) {
                reportVacHigh();
            }
        }

        if (reported.vacHigh != null) {
            vacHigh = reported.vacHigh;
            if (!carpetBoost) {
                // Can be overridden by "carpetBoost":true
                reportVacHigh();
            }
        }

        if (reported.noAutoPasses != null) {
            autoPasses = !reported.noAutoPasses;
            if (!reported.noAutoPasses) {
                // When set to false, overrides twoPass
                final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
                reportState(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_CLEAN_PASSES),
                        StringType.valueOf(PASSES_AUTO));
            } else if (twoPasses != null) {
                reportTwoPasses();
            }
        }

        if (reported.twoPass != null) {
            twoPasses = reported.twoPass;
            if (!autoPasses) {
                // Can be overridden by "noAutoPasses":false
                reportTwoPasses();
            }
        }

        if (reported.lastCommand != null) {
            final ChannelGroupUID internalGroupUID = new ChannelGroupUID(thingUID, INTERNAL_GROUP_ID);
            reportState(new ChannelUID(internalGroupUID, CHANNEL_INTERNAL_LAST_COMMAND),
                    StringType.valueOf(reported.lastCommand.toString()));
        }

        if (reported.mapUploadAllowed != null) {
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            reportState(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_MAP_UPLOAD),
                    OnOffType.from(reported.mapUploadAllowed));
        }

        reportProperty(Thing.PROPERTY_FIRMWARE_VERSION, reported.softwareVer);
        reportProperty("navSwVer", reported.navSwVer);
        reportProperty("wifiSwVer", reported.wifiSwVer);
        reportProperty("mobilityVer", reported.mobilityVer);
        reportProperty("bootloaderVer", reported.bootloaderVer);
        reportProperty("umiVer", reported.umiVer);
        reportProperty("sku", reported.sku);
        reportProperty("batteryType", reported.batteryType);

        if (reported.subModSwVer != null) {
            // This is used by i7 model. It has more capabilities, perhaps a dedicated
            // handler should be written by someone who owns it.
            reportProperty("subModSwVer.nav", reported.subModSwVer.nav);
            reportProperty("subModSwVer.mob", reported.subModSwVer.mob);
            reportProperty("subModSwVer.pwr", reported.subModSwVer.pwr);
            reportProperty("subModSwVer.sft", reported.subModSwVer.sft);
            reportProperty("subModSwVer.mobBtl", reported.subModSwVer.mobBtl);
            reportProperty("subModSwVer.linux", reported.subModSwVer.linux);
            reportProperty("subModSwVer.con", reported.subModSwVer.con);
        }
    }

    private void reportVacHigh() {
        final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thing.getUID(), CONTROL_GROUP_ID);
        reportState(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_POWER_BOOST),
                StringType.valueOf(vacHigh ? BOOST_PERFORMANCE : BOOST_ECO));
    }

    private void reportTwoPasses() {
        final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thing.getUID(), CONTROL_GROUP_ID);
        reportState(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_CLEAN_PASSES),
                StringType.valueOf(twoPasses ? PASSES_2 : PASSES_1));
    }

    private void reportState(final ChannelUID channel, State value) {
        lastState.put(channel, value);
        updateState(channel, value);
    }

    private void reportProperty(String property, @Nullable String value) {
        if (value != null) {
            updateProperty(property, value);
        }
    }
}
