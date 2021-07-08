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
import static org.openhab.core.thing.ThingStatus.INITIALIZING;
import static org.openhab.core.thing.ThingStatus.OFFLINE;
import static org.openhab.core.thing.ThingStatus.UNINITIALIZED;

import java.io.IOException;
import java.io.StringReader;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.List;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.irobot.internal.config.IRobotConfiguration;
import org.openhab.binding.irobot.internal.dto.BatteryLoad;
import org.openhab.binding.irobot.internal.dto.BinPause;
import org.openhab.binding.irobot.internal.dto.BinStatus;
import org.openhab.binding.irobot.internal.dto.CleanMissionStatus;
import org.openhab.binding.irobot.internal.dto.CleanPasses;
import org.openhab.binding.irobot.internal.dto.CleanSchedule;
import org.openhab.binding.irobot.internal.dto.IRobotDTO;
import org.openhab.binding.irobot.internal.dto.LastCommand;
import org.openhab.binding.irobot.internal.dto.MQTTProtocol;
import org.openhab.binding.irobot.internal.dto.MapUpload;
import org.openhab.binding.irobot.internal.dto.OpenOnly;
import org.openhab.binding.irobot.internal.dto.Pose;
import org.openhab.binding.irobot.internal.dto.PowerBoost;
import org.openhab.binding.irobot.internal.dto.Reported;
import org.openhab.binding.irobot.internal.dto.Root;
import org.openhab.binding.irobot.internal.dto.Signal;
import org.openhab.binding.irobot.internal.utils.LoginRequester;
import org.openhab.core.io.transport.mqtt.MqttConnectionState;
import org.openhab.core.library.types.DateTimeType;
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
import org.openhab.core.types.State;
import org.openhab.core.types.UnDefType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;
import com.google.gson.JsonParseException;
import com.google.gson.stream.JsonReader;

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

    private Hashtable<ChannelUID, IRobotDTO> lastState = new Hashtable<>();
    private boolean isPaused = false;

    private @Nullable Future<?> credentialRequester;
    protected IRobotConnectionHandler connection = new IRobotConnectionHandler() {
        @Override
        public void receive(final String topic, final String json) {
            if (json != null) {
                if (logger.isTraceEnabled()) {
                    logger.trace("Got topic {} data {}", topic, json);
                }
                RoombaHandler.this.receive(topic, json);

                final Root root = gson.fromJson(json, Root.class);
                if (root.getState() != null) {
                    Reported reported = root.getState().getReported();
                    if (reported != null) {
                        RoombaHandler.this.receive(reported);
                        final ChannelGroupUID stateGroupUID = new ChannelGroupUID(thing.getUID(), STATE_GROUP_ID);
                        updateState(new ChannelUID(stateGroupUID, CHANNEL_JSON), StringType.valueOf(json));
                    }
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

    public RoombaHandler(Thing thing) {
        super(thing);
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
            final IRobotDTO cache = lastState.get(channelUID);
            if (cache instanceof BatteryLoad) {
                final BatteryLoad batteryLoad = (BatteryLoad) cache;
                if (CHANNEL_STATE_CHARGE.equals(channelId)) {
                    final BigDecimal load = batteryLoad.getBatPct();
                    updateState(channelUID, load != null ? new DecimalType(load.longValue()) : UnDefType.UNDEF);
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for bin pause values.", channelUID);
                }
            } else if (cache instanceof BinPause) {
                final BinPause binPause = (BinPause) cache;
                if (CHANNEL_CONTROL_ALWAYS_FINISH.equals(channelId)) {
                    final Boolean pause = binPause.getBinPause();
                    updateState(channelUID, pause != null ? OnOffType.from(!pause) : UnDefType.UNDEF);
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for bin pause values.", channelUID);
                }
            } else if (cache instanceof BinStatus) {
                final BinStatus binStatus = (BinStatus) cache;
                if (CHANNEL_STATE_BIN.equals(channelId)) {
                    // The bin cannot be both full and removed simultaneously, so let's encode it as a single value
                    if (binStatus.getFull()) {
                        updateState(channelUID, StringType.valueOf(STATE_BIN_FULL));
                    } else if (binStatus.getPresent()) {
                        updateState(channelUID, StringType.valueOf(STATE_BIN_OK));
                    } else {
                        updateState(channelUID, StringType.valueOf(STATE_BIN_REMOVED));
                    }
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for clean passes values.", channelUID);
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
                        updateState(channelUID, StringType.valueOf(COMMAND_STOP));
                    } else {
                        switch (phase) {
                            case "pause": // Never observed in Roomba 930
                            case "stop":
                            case "stuck": { // CHECKME: could also be equivalent to "stop" command
                                isPaused = Boolean.TRUE;
                                updateState(channelUID, StringType.valueOf(COMMAND_PAUSE));
                                break;
                            }
                            case "dock": // Never observed in Roomba 930
                            case "hmUsrDock": {
                                isPaused = Boolean.FALSE;
                                updateState(channelUID, StringType.valueOf(COMMAND_DOCK));
                                break;
                            }
                            default: {
                                isPaused = Boolean.FALSE;
                                updateState(channelUID, cycle != null ? StringType.valueOf(cycle) : UnDefType.UNDEF);
                                break;
                            }
                        }
                    }
                } else if (CHANNEL_MISSION_CYCLE.equals(channelId)) {
                    final String cycle = missionStatus.getCycle();
                    updateState(channelUID, cycle != null ? StringType.valueOf(cycle) : UnDefType.UNDEF);
                } else if (CHANNEL_MISSION_ERROR.equals(channelId)) {
                    final BigInteger error = missionStatus.getError();
                    updateState(channelUID, error != null ? StringType.valueOf(error.toString()) : UnDefType.UNDEF);
                } else if (CHANNEL_MISSION_PHASE.equals(channelId)) {
                    final String phase = missionStatus.getPhase();
                    updateState(channelUID, phase != null ? StringType.valueOf(phase) : UnDefType.UNDEF);
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for clean passes values.", channelUID);
                }
            } else if (cache instanceof CleanPasses) {
                final CleanPasses passes = (CleanPasses) cache;
                if (CHANNEL_CONTROL_CLEAN_PASSES.equals(channelId)) {
                    // To make the life more interesting, paired values may not appear together in the same message.
                    if (Boolean.FALSE.equals(passes.getNoAutoPasses())) {
                        // When set to false, overrides twoPass
                        updateState(channelUID, StringType.valueOf(PASSES_AUTO));
                    } else {
                        if (Boolean.FALSE.equals(passes.getTwoPass())) {
                            updateState(channelUID, StringType.valueOf(PASSES_1));
                        } else if (Boolean.TRUE.equals(passes.getTwoPass())) {
                            updateState(channelUID, StringType.valueOf(PASSES_2));
                        } else {
                            boolean isNoAutoPassesSet = (passes.getNoAutoPasses() != null);
                            updateState(channelUID, isNoAutoPassesSet ? StringType.valueOf(PASSES_1) : UnDefType.UNDEF);
                        }
                    }
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for clean passes values.", channelUID);
                }
            } else if (cache instanceof CleanSchedule) {
                final CleanSchedule schedule = (CleanSchedule) cache;
                int index = Arrays.asList(DAY_OF_WEEK).indexOf(channelId.substring(0, channelId.indexOf("_")));
                if (channelId.endsWith("_enabled")) {
                    State state = UnDefType.UNDEF;
                    final List<String> cycle = schedule.getCycle();
                    try {
                        state = OnOffType.from("start".equals(cycle.get(index)));
                    } catch (IndexOutOfBoundsException exception) {
                        // Eat exception, since UNDEF was already set
                    }
                    updateState(channelUID, state);
                } else if (channelId.endsWith("_time")) {
                    State state = UnDefType.UNDEF;
                    final List<BigInteger> hour = schedule.getH();
                    final List<BigInteger> minutes = schedule.getM();
                    try {
                        // state = new DateTimeType(
                    } catch (IndexOutOfBoundsException exception) {
                        // Eat exception, since UNDEF was already set
                    }
                    updateState(channelUID, state);
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for schedule values.", channelUID);
                }
            } else if (cache instanceof MapUpload) {
                final MapUpload mapUpload = (MapUpload) cache;
                if (CHANNEL_CONTROL_MAP_UPLOAD.equals(channelId)) {
                    final Boolean upload = mapUpload.getMapUploadAllowed();
                    updateState(channelUID, upload != null ? OnOffType.from(upload) : UnDefType.UNDEF);
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for map upload values.", channelUID);
                }
            } else if (cache instanceof OpenOnly) {
                final OpenOnly openOnly = (OpenOnly) cache;
                if (CHANNEL_CONTROL_EDGE_CLEAN.equals(channelId)) {
                    final Boolean open = openOnly.getOpenOnly();
                    updateState(channelUID, open != null ? OnOffType.from(!open) : UnDefType.UNDEF);
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for edge clean values.", channelUID);
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
            } else if (cache instanceof PowerBoost) {
                final PowerBoost boost = (PowerBoost) cache;
                if (CHANNEL_CONTROL_POWER_BOOST.equals(channelId)) {
                    if (Boolean.TRUE.equals(boost.getCarpetBoost())) {
                        // When set to true, overrides vacHigh
                        updateState(channelUID, StringType.valueOf(BOOST_AUTO));
                    } else if (Boolean.TRUE.equals(boost.getVacHigh())) {
                        updateState(channelUID, StringType.valueOf(BOOST_PERFORMANCE));
                    } else if (Boolean.FALSE.equals(boost.getVacHigh())) {
                        updateState(channelUID, StringType.valueOf(BOOST_ECO));
                    } else {
                        updateState(channelUID, UnDefType.UNDEF);
                    }
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for boost values.", channelUID);
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
            } else {
                updateState(channelUID, UnDefType.UNDEF);
            }
        } else if (command instanceof DateTimeType) {
            if (SCHEDULE_GROUP_ID.equals(channelUID.getGroupId())) {
                /*
                 * for (int i = 0; i < DAY_OF_WEEK.length; i++) {
                 * final IRobotDTO cache = newLastState.get(new ChannelUID(SCHEDULE_GROUP_ID, DAY_OF_WEEK[i] +
                 * "_enabled"));
                 * final IRobotDTO cache = newLastState.get(new ChannelUID(SCHEDULE_GROUP_ID, DAY_OF_WEEK[i] +
                 * "_time"));
                 * }
                 * final CleanSchedule state = new CleanSchedule();
                 * state.setCycle();
                 * state.setH();
                 * state.setM();
                 * connection.send("delta", "{\"state\":" + gson.toJson(state) + "}");
                 */
            } else if (logger.isTraceEnabled()) {
                logger.trace("Received {} for channel {}.", command, channelId);
            }
        } else if (command instanceof OnOffType) {
            if (CHANNEL_CONTROL_ALWAYS_FINISH.equals(channelId)) {
                final BinPause state = new BinPause();
                state.setBinPause(command.equals(OnOffType.OFF));
                connection.send("delta", "{\"state\":" + gson.toJson(state) + "}");
            } else if (CHANNEL_CONTROL_EDGE_CLEAN.equals(channelId)) {
                final OpenOnly state = new OpenOnly();
                state.setOpenOnly(command.equals(OnOffType.OFF));
                connection.send("delta", "{\"state\":" + gson.toJson(state) + "}");
            } else if (CHANNEL_CONTROL_MAP_UPLOAD.equals(channelId)) {
                final MapUpload state = new MapUpload();
                state.setMapUploadAllowed(command.equals(OnOffType.ON));
                connection.send("delta", "{\"state\":" + gson.toJson(state) + "}");
            } else if (SCHEDULE_GROUP_ID.equals(channelUID.getGroupId())) {
                String[] cycle = new String[DAY_OF_WEEK.length];
                BigInteger[] hours = new BigInteger[DAY_OF_WEEK.length];
                BigInteger[] minutes = new BigInteger[DAY_OF_WEEK.length];
                final ChannelGroupUID groupUID = new ChannelGroupUID(channelUID.getThingUID(), SCHEDULE_GROUP_ID);
                for (int i = 0; i < DAY_OF_WEEK.length; i++) {
                    final ChannelUID cycleChannelUID = new ChannelUID(groupUID, DAY_OF_WEEK[i] + "_enabled");
                    final CleanSchedule enabled = (CleanSchedule) lastState.get(cycleChannelUID);
                    cycle[i] = enabled.getCycle().get(i);
                    final ChannelUID timeChannelUID = new ChannelUID(groupUID, DAY_OF_WEEK[i] + "_time");
                    final CleanSchedule time = (CleanSchedule) lastState.get(timeChannelUID);
                    hours[i] = time.getH().get(i);
                    minutes[i] = time.getM().get(i);
                }
                final CleanSchedule state = new CleanSchedule();
                state.setCycle(Arrays.asList(cycle));
                state.setH(Arrays.asList(hours));
                state.setM(Arrays.asList(minutes));
                // connection.send("delta", "{\"state\":" + gson.toJson(state) + "}");
            } else if (logger.isTraceEnabled()) {
                logger.trace("Received {} for channel {}.", command, channelId);
            }
        } else if (command instanceof StringType) {
            if (CHANNEL_CONTROL_CLEAN_PASSES.equals(channelId)) {
                final CleanPasses state = new CleanPasses();
                state.setNoAutoPasses(!command.equals(PASSES_AUTO));
                state.setTwoPass(command.equals(PASSES_2));
                connection.send("delta", "{\"state\":" + gson.toJson(state) + "}");
            } else if (CHANNEL_CONTROL_POWER_BOOST.equals(channelId)) {
                final PowerBoost state = new PowerBoost();
                state.setCarpetBoost(command.equals(BOOST_AUTO));
                state.setVacHigh(command.equals(BOOST_PERFORMANCE));
                connection.send("delta", "{\"state\":" + gson.toJson(state) + "}");
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

    private void refresh(ChannelUID channelUID, IRobotDTO object) {
        lastState.put(channelUID, object);
        handleCommand(channelUID, RefreshType.REFRESH);
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

    public void receive(final Reported reported) {
        final ThingUID thingUID = thing.getUID();

        final BigDecimal batLoad = reported.getBatPct();
        if (batLoad != null) {
            final ChannelGroupUID stateGroupUID = new ChannelGroupUID(thingUID, STATE_GROUP_ID);
            BatteryLoad load = new BatteryLoad();
            load.setBatPct(batLoad);
            refresh(new ChannelUID(stateGroupUID, CHANNEL_STATE_CHARGE), load);
        }

        final Boolean binPause = reported.getBinPause();
        if (binPause != null) {
            final BinPause pause = new BinPause();
            pause.setBinPause(binPause);
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            refresh(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_ALWAYS_FINISH), pause);
        }

        final BinStatus binStatus = reported.getBin();
        if (binStatus != null) {
            final ChannelGroupUID stateGroupUID = new ChannelGroupUID(thingUID, STATE_GROUP_ID);
            refresh(new ChannelUID(stateGroupUID, CHANNEL_STATE_BIN), binStatus);
        }

        final CleanMissionStatus missionStatus = reported.getCleanMissionStatus();
        if (missionStatus != null) {
            final ChannelGroupUID missionGroupUID = new ChannelGroupUID(thingUID, MISSION_GROUP_ID);
            refresh(new ChannelUID(missionGroupUID, CHANNEL_AREA), missionStatus);
            refresh(new ChannelUID(missionGroupUID, CHANNEL_MISSION_CYCLE), missionStatus);
            refresh(new ChannelUID(missionGroupUID, CHANNEL_MISSION_ERROR), missionStatus);
            refresh(new ChannelUID(missionGroupUID, CHANNEL_MISSION_PHASE), missionStatus);

            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            refresh(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_COMMAND), missionStatus);
        }

        final CleanSchedule schedule = reported.getCleanSchedule();
        if (schedule != null) {
            final ChannelGroupUID scheduleGroupUID = new ChannelGroupUID(thingUID, SCHEDULE_GROUP_ID);
            for (int i = 0; i < DAY_OF_WEEK.length; i++) {
                refresh(new ChannelUID(scheduleGroupUID, DAY_OF_WEEK[i] + "_enabled"), schedule);
                refresh(new ChannelUID(scheduleGroupUID, DAY_OF_WEEK[i] + "_time"), schedule);
            }
        }

        final LastCommand lastCommand = reported.getLastCommand();
        if (lastCommand != null) {
            final ChannelGroupUID internalGroupUID = new ChannelGroupUID(thingUID, INTERNAL_GROUP_ID);
            lastState.put(new ChannelUID(internalGroupUID, CHANNEL_INTERNAL_LAST_COMMAND), lastCommand);
        }

        final Boolean mapUploadAllowed = reported.getMapUploadAllowed();
        if (mapUploadAllowed != null) {
            MapUpload uploadAllowed = new MapUpload();
            uploadAllowed.setMapUploadAllowed(mapUploadAllowed);
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            refresh(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_MAP_UPLOAD), uploadAllowed);
        }

        final Boolean openOnly = reported.getOpenOnly();
        if (openOnly != null) {
            OpenOnly open = new OpenOnly();
            open.setOpenOnly(openOnly);
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            refresh(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_EDGE_CLEAN), open);
        }

        final Pose pose = reported.getPose();
        if (pose != null) {
            final ChannelGroupUID positionGroupUID = new ChannelGroupUID(thingUID, POSITION_GROUP_ID);
            refresh(new ChannelUID(positionGroupUID, CHANNEL_POSITION_THETA), pose);
            refresh(new ChannelUID(positionGroupUID, CHANNEL_POSITION_X), pose);
            refresh(new ChannelUID(positionGroupUID, CHANNEL_POSITION_Y), pose);
        }

        final Signal signal = reported.getSignal();
        if (signal != null) {
            final ChannelGroupUID networkGroupUID = new ChannelGroupUID(thingUID, NETWORK_GROUP_ID);
            refresh(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_RSSI), signal);
            refresh(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_SNR), signal);
            refresh(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_NOISE), signal);
        }

        final Boolean vacHigh = reported.getVacHigh();
        final Boolean carpetBoost = reported.getCarpetBoost();
        if ((carpetBoost != null) || (vacHigh != null)) {
            // "vacHigh":false, "carpetBoost":false
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            final IRobotDTO cached = lastState.get(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_POWER_BOOST));
            PowerBoost boost = cached instanceof PowerBoost ? (PowerBoost) cached : new PowerBoost();
            boost.setCarpetBoost(carpetBoost);
            boost.setVacHigh(vacHigh);
            refresh(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_POWER_BOOST), boost);
        }

        final Boolean twoPass = reported.getTwoPass();
        final Boolean noAutoPasses = reported.getNoAutoPasses();
        if ((noAutoPasses != null) || (twoPass != null)) {
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            final IRobotDTO cached = lastState.get(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_CLEAN_PASSES));
            CleanPasses passes = cached instanceof CleanPasses ? (CleanPasses) cached : new CleanPasses();
            if (noAutoPasses != null) {
                passes.setNoAutoPasses(noAutoPasses);
            }
            if (twoPass != null) {
                passes.setTwoPass(twoPass);
            }
            refresh(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_CLEAN_PASSES), passes);
        }
    }

    public void receive(final String topic, final String json) {
        MQTTProtocol.StateMessage msg;

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
        if (msg.state != null && msg.state.reported != null) {
            MQTTProtocol.GenericState reported = msg.state.reported;

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
    }

    private void reportProperty(String property, @Nullable String value) {
        if (value != null) {
            updateProperty(property, value);
        }
    }
}
