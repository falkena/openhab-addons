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
import static org.openhab.core.library.unit.ImperialUnits.SQUARE_FOOT;
import static org.openhab.core.thing.Thing.PROPERTY_FIRMWARE_VERSION;
import static org.openhab.core.thing.Thing.PROPERTY_MODEL_ID;
import static org.openhab.core.thing.ThingStatus.INITIALIZING;
import static org.openhab.core.thing.ThingStatus.OFFLINE;
import static org.openhab.core.thing.ThingStatus.UNINITIALIZED;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Set;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.irobot.internal.IRobotChannelContentProvider;
import org.openhab.binding.irobot.internal.config.IRobotConfiguration;
import org.openhab.binding.irobot.internal.dto.BatteryLoad;
import org.openhab.binding.irobot.internal.dto.CleanMissionStatus;
import org.openhab.binding.irobot.internal.dto.CleanPasses;
import org.openhab.binding.irobot.internal.dto.IRobotDTO;
import org.openhab.binding.irobot.internal.dto.LastCommand;
import org.openhab.binding.irobot.internal.dto.MQTTProtocol;
import org.openhab.binding.irobot.internal.dto.MapUpload;
import org.openhab.binding.irobot.internal.dto.Name;
import org.openhab.binding.irobot.internal.dto.Pose;
import org.openhab.binding.irobot.internal.dto.Reported;
import org.openhab.binding.irobot.internal.dto.Root;
import org.openhab.binding.irobot.internal.dto.Signal;
import org.openhab.binding.irobot.internal.dto.Timezone;
import org.openhab.binding.irobot.internal.utils.LoginRequester;
import org.openhab.core.io.transport.mqtt.MqttConnectionState;
import org.openhab.core.library.types.DecimalType;
import org.openhab.core.library.types.OnOffType;
import org.openhab.core.library.types.QuantityType;
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
import org.openhab.core.types.UnDefType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;

/**
 * The {@link IRobotCommonHandler} is responsible for handling commands, which are
 * sent to one of the channels.
 *
 * @author hkuhn42 - Initial contribution
 * @author Pavel Fedin - Rewrite for 900 series
 * @author Florian Binder - added cleanRegions command and lastCommand channel
 * @author Alexander Falkenstern - Add support for I7 series
 */
@NonNullByDefault
public class IRobotCommonHandler extends BaseThingHandler {
    String PROPERTY_BATTERY_TYPE = "batteryType";

    private final Logger logger = LoggerFactory.getLogger(IRobotCommonHandler.class);

    private final Gson gson = new Gson();

    private IRobotChannelContentProvider channelContentProvider;

    private Set<String> filledProperties = new HashSet<>();
    private Hashtable<ChannelUID, IRobotDTO> lastState = new Hashtable<>();
    private boolean isPaused = false;

    private @Nullable Future<?> credentialRequester;
    protected IRobotConnectionHandler connection = new IRobotConnectionHandler() {
        @Override
        public void receive(final String topic, final String json) {
            if (logger.isTraceEnabled()) {
                logger.trace("Got topic {} data {}", topic, json);
            }

            final Root root = gson.fromJson(json, Root.class);
            if (root.getState() != null) {
                Reported reported = root.getState().getReported();
                if (reported != null) {
                    IRobotCommonHandler.this.receive(reported);
                    final ChannelGroupUID stateGroupUID = new ChannelGroupUID(thing.getUID(), STATE_GROUP_ID);
                    updateState(new ChannelUID(stateGroupUID, CHANNEL_JSON), json);
                }
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

    public IRobotCommonHandler(Thing thing, IRobotChannelContentProvider channelContentProvider) {
        super(thing);
        this.channelContentProvider = channelContentProvider;
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
        final String channelId = channelUID.getIdWithoutGroup();
        if (command instanceof RefreshType) {
            final IRobotDTO cache = getCacheEntry(channelUID);
            if (cache instanceof BatteryLoad) {
                final BatteryLoad batteryLoad = (BatteryLoad) cache;
                if (CHANNEL_STATE_CHARGE.equals(channelId)) {
                    final BigInteger load = batteryLoad.getBatPct();
                    updateState(channelUID, load != null ? new DecimalType(load.longValue()) : UnDefType.UNDEF);
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for bin pause values.", channelUID);
                }
            } else if (cache instanceof CleanMissionStatus) {
                final CleanMissionStatus missionStatus = (CleanMissionStatus) cache;
                if (CHANNEL_AREA.equals(channelId)) {
                    final BigDecimal area = missionStatus.getSqft();
                    updateState(channelUID, area != null ? new QuantityType<>(area, SQUARE_FOOT) : UnDefType.UNDEF);
                } else if (CHANNEL_CONTROL_COMMAND.equals(channelId)) {
                    final String cycle = missionStatus.getCycle();
                    final String phase = missionStatus.getPhase();
                    if ("none".equalsIgnoreCase(cycle)) {
                        isPaused = Boolean.FALSE;
                        updateState(channelUID, COMMAND_STOP);
                    } else {
                        switch (phase) {
                            case "pause": // Never observed in Roomba 930
                            case "stop":
                            case "stuck": { // CHECKME: could also be equivalent to "stop" command
                                isPaused = Boolean.TRUE;
                                updateState(channelUID, COMMAND_PAUSE);
                                break;
                            }
                            case "dock": // Never observed in Roomba 930
                            case "hmUsrDock": {
                                isPaused = Boolean.FALSE;
                                updateState(channelUID, COMMAND_DOCK);
                                break;
                            }
                            default: {
                                isPaused = Boolean.FALSE;
                                updateState(channelUID, cycle);
                                break;
                            }
                        }
                    }
                } else if (CHANNEL_MISSION_CYCLE.equals(channelId)) {
                    updateState(channelUID, missionStatus.getCycle());
                } else if (CHANNEL_MISSION_ERROR.equals(channelId)) {
                    final BigInteger error = missionStatus.getError();
                    updateState(channelUID, error != null ? StringType.valueOf(error.toString()) : UnDefType.UNDEF);
                } else if (CHANNEL_MISSION_PHASE.equals(channelId)) {
                    updateState(channelUID, missionStatus.getPhase());
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for clean passes values.", channelUID);
                }
            } else if (cache instanceof CleanPasses) {
                final CleanPasses passes = (CleanPasses) cache;
                if (CHANNEL_CONTROL_CLEAN_PASSES.equals(channelId)) {
                    // To make the life more interesting, paired values may not appear together in the same message.
                    if (Boolean.FALSE.equals(passes.getNoAutoPasses())) {
                        // When set to false, overrides twoPass
                        updateState(channelUID, PASSES_AUTO);
                    } else {
                        if (Boolean.FALSE.equals(passes.getTwoPass())) {
                            updateState(channelUID, PASSES_1);
                        } else if (Boolean.TRUE.equals(passes.getTwoPass())) {
                            updateState(channelUID, PASSES_2);
                        } else {
                            boolean isNoAutoPassesSet = (passes.getNoAutoPasses() != null);
                            updateState(channelUID, isNoAutoPassesSet ? StringType.valueOf(PASSES_1) : UnDefType.UNDEF);
                        }
                    }
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for clean passes values.", channelUID);
                }
            } else if (cache instanceof MapUpload) {
                final MapUpload mapUpload = (MapUpload) cache;
                if (CHANNEL_CONTROL_MAP_UPLOAD.equals(channelId)) {
                    final Boolean upload = mapUpload.getMapUploadAllowed();
                    updateState(channelUID, upload != null ? OnOffType.from(upload) : UnDefType.UNDEF);
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for map upload values.", channelUID);
                }
            } else if (cache instanceof Name) {
                final Name name = (Name) cache;
                if (CHANNEL_COMMON_NAME.equals(channelId)) {
                    updateState(channelUID, name.getName());
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for map upload values.", channelUID);
                }
            } else if (cache instanceof Pose) {
                final Pose pose = (Pose) cache;
                if (CHANNEL_POSITION_THETA.equals(channelId)) {
                    final BigDecimal theta = pose.getTheta();
                    updateState(channelUID, theta != null ? new DecimalType(theta.longValue()) : UnDefType.UNDEF);
                } else if (CHANNEL_POSITION_X.equals(channelId)) {
                    final BigDecimal x = pose.getPoint().getX();
                    updateState(channelUID, x != null ? new DecimalType(x.longValue()) : UnDefType.UNDEF);
                } else if (CHANNEL_POSITION_Y.equals(channelId)) {
                    final BigDecimal y = pose.getPoint().getY();
                    updateState(channelUID, y != null ? new DecimalType(y.longValue()) : UnDefType.UNDEF);
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for pose values.", channelUID);
                }
            } else if (cache instanceof Signal) {
                final Signal signal = (Signal) cache;
                if (CHANNEL_NETWORK_RSSI.equals(channelId)) {
                    final BigInteger rssi = signal.getRssi();
                    updateState(channelUID, rssi != null ? new DecimalType(rssi.longValue()) : UnDefType.UNDEF);
                } else if (CHANNEL_NETWORK_SNR.equals(channelId)) {
                    final BigInteger snr = signal.getSnr();
                    updateState(channelUID, snr != null ? new DecimalType(snr.longValue()) : UnDefType.UNDEF);
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for signal values.", channelUID);
                }
            } else if (cache instanceof Timezone) {
                final Timezone timezone = (Timezone) cache;
                if (CHANNEL_COMMON_TIMEZONE.equals(channelId)) {
                    updateState(channelUID, timezone.getTimezone());
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for map upload values.", channelUID);
                }
            } else {
                updateState(channelUID, UnDefType.UNDEF);
            }
        } else if (command instanceof OnOffType) {
            if (CHANNEL_CONTROL_MAP_UPLOAD.equals(channelId)) {
                sendSetting(new MapUpload(command.equals(OnOffType.ON)));
            } else if (logger.isTraceEnabled()) {
                logger.trace("Received {} for channel {}.", command, channelId);
            }
        } else if (command instanceof StringType) {
            if (CHANNEL_CONTROL_CLEAN_PASSES.equals(channelId)) {
                sendSetting(new CleanPasses(!command.equals(PASSES_AUTO), command.equals(PASSES_2)));
            } else if (CHANNEL_COMMON_NAME.equals(channelId)) {
                sendSetting(new Name(command.toString()));
            } else if (logger.isTraceEnabled()) {
                logger.trace("Received {} for channel {}.", command, channelId);
            }
        }

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
        }
    }

    protected void updateState(ChannelUID channelUID, @Nullable String value) {
        updateState(channelUID, value != null ? StringType.valueOf(value.trim()) : UnDefType.UNDEF);
    }

    @Override
    protected void updateProperty(String name, @Nullable String value) {
        if (value != null) {
            super.updateProperty(name, value);
            filledProperties.add(name);
        } else if (!filledProperties.contains(name)) {
            super.updateProperty(name, UNKNOWN);
        }
    }

    protected void setCacheEntry(final ChannelUID channelUID, final IRobotDTO value) {
        lastState.put(channelUID, value);
        handleCommand(channelUID, RefreshType.REFRESH);
    }

    protected @Nullable IRobotDTO getCacheEntry(final ChannelUID channelUID) {
        return lastState.get(channelUID);
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

    protected void receive(final Reported reported) {
        final ThingUID thingUID = thing.getUID();

        final BigInteger batLoad = reported.getBatPct();
        if (batLoad != null) {
            final BatteryLoad load = new BatteryLoad(batLoad);
            final ChannelGroupUID stateGroupUID = new ChannelGroupUID(thingUID, STATE_GROUP_ID);
            setCacheEntry(new ChannelUID(stateGroupUID, CHANNEL_STATE_CHARGE), load);
        }

        final CleanMissionStatus missionStatus = reported.getCleanMissionStatus();
        if (missionStatus != null) {
            final ChannelGroupUID missionGroupUID = new ChannelGroupUID(thingUID, MISSION_GROUP_ID);
            setCacheEntry(new ChannelUID(missionGroupUID, CHANNEL_AREA), missionStatus);
            setCacheEntry(new ChannelUID(missionGroupUID, CHANNEL_MISSION_CYCLE), missionStatus);
            setCacheEntry(new ChannelUID(missionGroupUID, CHANNEL_MISSION_ERROR), missionStatus);
            setCacheEntry(new ChannelUID(missionGroupUID, CHANNEL_MISSION_PHASE), missionStatus);

            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            setCacheEntry(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_COMMAND), missionStatus);
        }

        final LastCommand lastCommand = reported.getLastCommand();
        if (lastCommand != null) {
            final ChannelGroupUID internalGroupUID = new ChannelGroupUID(thingUID, INTERNAL_GROUP_ID);
            lastState.put(new ChannelUID(internalGroupUID, CHANNEL_INTERNAL_LAST_COMMAND), lastCommand);
        }

        final Boolean mapUploadAllowed = reported.getMapUploadAllowed();
        if (mapUploadAllowed != null) {
            final MapUpload uploadAllowed = new MapUpload(mapUploadAllowed);
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            setCacheEntry(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_MAP_UPLOAD), uploadAllowed);
        }

        final String name = reported.getName();
        if (name != null) {
            final Name robotName = new Name(name);
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, COMMON_GROUP_ID);
            setCacheEntry(new ChannelUID(controlGroupUID, CHANNEL_COMMON_NAME), robotName);
        }

        final Pose pose = reported.getPose();
        if (pose != null) {
            final ChannelGroupUID positionGroupUID = new ChannelGroupUID(thingUID, POSITION_GROUP_ID);
            setCacheEntry(new ChannelUID(positionGroupUID, CHANNEL_POSITION_THETA), pose);
            setCacheEntry(new ChannelUID(positionGroupUID, CHANNEL_POSITION_X), pose);
            setCacheEntry(new ChannelUID(positionGroupUID, CHANNEL_POSITION_Y), pose);
        }

        final Signal signal = reported.getSignal();
        if (signal != null) {
            final ChannelGroupUID networkGroupUID = new ChannelGroupUID(thingUID, NETWORK_GROUP_ID);
            setCacheEntry(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_RSSI), signal);
            setCacheEntry(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_SNR), signal);
        }

        final String timezone = reported.getTimezone();
        if (timezone != null) {
            final ChannelGroupUID commonGroupUID = new ChannelGroupUID(thingUID, COMMON_GROUP_ID);
            final ChannelUID timeZoneChannelUID = new ChannelUID(commonGroupUID, CHANNEL_COMMON_TIMEZONE);
            if (!channelContentProvider.isChannelPopulated(timeZoneChannelUID)) {
                channelContentProvider.setTimeZones(timeZoneChannelUID);
            }
            setCacheEntry(timeZoneChannelUID, new Timezone(timezone));
        }

        final Boolean twoPass = reported.getTwoPass();
        final Boolean noAutoPasses = reported.getNoAutoPasses();
        if ((noAutoPasses != null) || (twoPass != null)) {
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            final IRobotDTO cached = getCacheEntry(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_CLEAN_PASSES));
            CleanPasses passes = cached instanceof CleanPasses ? (CleanPasses) cached : new CleanPasses();

            if (noAutoPasses != null) {
                passes.setNoAutoPasses(noAutoPasses);
            }
            if (twoPass != null) {
                passes.setTwoPass(twoPass);
            }
            setCacheEntry(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_CLEAN_PASSES), passes);
        }

        // Properties
        updateProperty(PROPERTY_BATTERY_TYPE, reported.getBatteryType());
        updateProperty(PROPERTY_FIRMWARE_VERSION, reported.getSoftwareVer());
        updateProperty(PROPERTY_MODEL_ID, reported.getSku());
    }

    protected void sendSetting(final IRobotDTO setting) {
        connection.send("delta", "{\"state\":" + gson.toJson(setting) + "}");
    }
}
