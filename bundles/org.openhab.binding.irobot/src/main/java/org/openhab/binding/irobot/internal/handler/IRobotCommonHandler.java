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
import static org.openhab.core.library.unit.ImperialUnits.SQUARE_FOOT;
import static org.openhab.core.library.unit.Units.MINUTE;
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
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.irobot.internal.IRobotChannelContentProvider;
import org.openhab.binding.irobot.internal.config.IRobotConfiguration;
import org.openhab.binding.irobot.internal.dto.BBMssn;
import org.openhab.binding.irobot.internal.dto.BBRun;
import org.openhab.binding.irobot.internal.dto.Bbsys;
import org.openhab.binding.irobot.internal.dto.BinState;
import org.openhab.binding.irobot.internal.dto.CleanMissionStatus;
import org.openhab.binding.irobot.internal.dto.IRobotDTO;
import org.openhab.binding.irobot.internal.dto.LastCommand;
import org.openhab.binding.irobot.internal.dto.NetInfo;
import org.openhab.binding.irobot.internal.dto.Pose;
import org.openhab.binding.irobot.internal.dto.Reported;
import org.openhab.binding.irobot.internal.dto.Root;
import org.openhab.binding.irobot.internal.dto.Signal;
import org.openhab.binding.irobot.internal.dto.WlanConfig;
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
import org.openhab.core.types.State;
import org.openhab.core.types.UnDefType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

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
    private Hashtable<ChannelUID, State> lastState = new Hashtable<>();

    private @Nullable Future<?> credentialRequester;
    protected IRobotConnectionHandler connection = new IRobotConnectionHandler() {
        @Override
        public void receive(final String topic, final String json) {
            if (logger.isTraceEnabled()) {
                logger.trace("Got topic {} data {}", topic, json);
            }

            @Nullable
            Reported reported = null;
            try {
                final Root root = gson.fromJson(json, Root.class);
                if ((root != null) && (root.getState() != null)) {
                    reported = root.getState().getReported();
                }
            } catch (JsonSyntaxException exception) {
                logger.warn("Failed to parse {} for {}: {}", json, thing.getLabel(), exception.toString());
            }

            if (reported != null) {
                IRobotCommonHandler.this.receive(reported);
                final ChannelGroupUID stateGroupUID = new ChannelGroupUID(thing.getUID(), STATE_GROUP_ID);
                updateState(new ChannelUID(stateGroupUID, CHANNEL_JSON), StringType.valueOf(json));
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

    protected void updateState(ChannelUID channelUID, @Nullable BigDecimal state) {
        if (state != null) {
            updateState(channelUID, new DecimalType(state));
        }
    }

    protected void updateState(ChannelUID channelUID, @Nullable BigInteger state) {
        if (state != null) {
            updateState(channelUID, new BigDecimal(state));
        }
    }

    protected void updateState(ChannelUID channelUID, @Nullable String state) {
        if (state != null) {
            updateState(channelUID, new StringType(state.trim()));
        }
    }

    @Override
    protected void updateState(ChannelUID channelUID, State state) {
        setCacheEntry(channelUID, state);
        super.updateState(channelUID, state);
    }

    protected void setCacheEntry(final ChannelUID channelUID, final State value) {
        lastState.put(channelUID, value);
    }

    protected @Nullable State getCacheEntry(final ChannelUID channelUID) {
        return lastState.get(channelUID);
    }

    protected void updateProperty(String property, @Nullable BigInteger value) {
        updateProperty(property, value != null ? value.toString() : UNKNOWN);
    }

    @Override
    protected void updateProperty(String property, @Nullable String value) {
        final String existing = thing.getProperties().get(property);
        if ((existing == null) || UNKNOWN.equals(existing)) {
            super.updateProperty(property, value != null ? value : UNKNOWN);
        }
    }

    @Override
    public void handleCommand(ChannelUID channelUID, Command command) {
        final String channelId = channelUID.getIdWithoutGroup();
        if (command instanceof RefreshType) {
            final @Nullable State value = getCacheEntry(channelUID);
            super.updateState(channelUID, value != null ? value : UnDefType.UNDEF);
        } else if (command instanceof OnOffType) {
            if (CHANNEL_CONTROL_ALWAYS_FINISH.equals(channelId)) {
                Reported request = new Reported();
                request.setBinPause(command.equals(OnOffType.OFF));
                sendSetting(request);
            } else if (CHANNEL_CONTROL_MAP_UPLOAD.equals(channelId)) {
                Reported request = new Reported();
                request.setMapUploadAllowed(command.equals(OnOffType.ON));
                sendSetting(request);
            }
        } else if (command instanceof StringType) {
            if (CHANNEL_CONTROL_CLEAN_PASSES.equals(channelId)) {
                Reported request = new Reported();
                request.setNoAutoPasses(!command.equals(PASSES_AUTO));
                request.setTwoPass(command.equals(PASSES_2));
                sendSetting(request);
            } else if (CHANNEL_CONTROL_COMMAND.equals(channelId)) {
                String request = command.toString();
                if (request.equals(COMMAND_CLEAN)) {
                    final @Nullable State cache = getCacheEntry(channelUID);
                    request = (cache != null) && cache.equals(COMMAND_PAUSE) ? "resume" : "start";
                }
                sendCommand(request, null);
            } else if (CHANNEL_CONTROL_LANGUAGE.equals(channelId)) {
                Reported request = new Reported();
                request.setLanguage(new BigInteger(command.toString()));
                sendSetting(request);
            } else if (CHANNEL_COMMON_NAME.equals(channelId)) {
                Reported request = new Reported();
                request.setName(command.toString());
                sendSetting(request);
            } else if (CHANNEL_COMMON_TIMEZONE.equals(channelId)) {
                Reported request = new Reported();
                request.setTimezone(command.toString());
                sendSetting(request);
            }
        }
    }

    protected void receive(final Reported reported) {
        final ThingUID thingUID = thing.getUID();

        final BigInteger batteryLoad = reported.getBatPct();
        if (batteryLoad != null) {
            final ChannelGroupUID stateGroupUID = new ChannelGroupUID(thingUID, STATE_GROUP_ID);
            updateState(new ChannelUID(stateGroupUID, CHANNEL_STATE_CHARGE), batteryLoad);
        }

        final BBMssn bbMssn = reported.getBbmssn();
        if (bbMssn != null) {
            final ChannelGroupUID commonGroupUID = new ChannelGroupUID(thingUID, COMMON_GROUP_ID);
            updateState(new ChannelUID(commonGroupUID, CHANNEL_COMMON_MISSION_COUNT), bbMssn.getnMssn());

        }

        final BBRun bbRun = reported.getBbrun();
        if (bbRun != null) {
            final ChannelGroupUID commonGroupUID = new ChannelGroupUID(thingUID, COMMON_GROUP_ID);

            final BigInteger sqft = bbRun.getSqft();
            if (sqft != null) {
                updateState(new ChannelUID(commonGroupUID, CHANNEL_COMMON_AREA), new QuantityType<>(sqft, SQUARE_FOOT));
            }

            final BigInteger hours = bbRun.getHr();
            final BigInteger minutes = bbRun.getMin();
            if ((hours != null) && (minutes != null)) {
                final long clean = 60 * hours.longValueExact() + minutes.longValueExact();
                updateState(new ChannelUID(commonGroupUID, CHANNEL_COMMON_DURATION), new QuantityType<>(clean, MINUTE));
            }

            updateState(new ChannelUID(commonGroupUID, CHANNEL_COMMON_SCRUBS_COUNT), bbRun.getnScrubs());
        }

        final Bbsys bbSys = reported.getBbsys();
        if (bbSys != null) {
            final BigInteger hours = bbSys.getHr();
            final BigInteger minutes = bbSys.getMin();
            if ((hours != null) && (minutes != null)) {
                final long uptime = 60 * hours.longValueExact() + minutes.longValueExact();
                final ChannelGroupUID commonGroupUID = new ChannelGroupUID(thingUID, COMMON_GROUP_ID);
                updateState(new ChannelUID(commonGroupUID, CHANNEL_COMMON_UPTIME), new QuantityType<>(uptime, MINUTE));
            }
        }

        final BinState binState = reported.getBin();
        if (binState != null) {
            final ChannelGroupUID stateGroupUID = new ChannelGroupUID(thingUID, STATE_GROUP_ID);
            // The bin cannot be both full and removed simultaneously, so let's encode it as a single value
            if (!binState.getPresent()) {
                updateState(new ChannelUID(stateGroupUID, CHANNEL_STATE_BIN), STATE_BIN_REMOVED);
            } else if (binState.getFull()) {
                updateState(new ChannelUID(stateGroupUID, CHANNEL_STATE_BIN), STATE_BIN_FULL);
            } else {
                updateState(new ChannelUID(stateGroupUID, CHANNEL_STATE_BIN), STATE_BIN_OK);
            }
        }

        final Boolean binPause = reported.getBinPause();
        if (binPause != null) {
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            updateState(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_ALWAYS_FINISH), OnOffType.from(!binPause));
        }

        final CleanMissionStatus missionStatus = reported.getCleanMissionStatus();
        if (missionStatus != null) {
            final ChannelGroupUID missionGroupUID = new ChannelGroupUID(thingUID, MISSION_GROUP_ID);

            String command;
            final String cycle = missionStatus.getCycle();
            final String phase = missionStatus.getPhase();
            if ("none".equals(cycle)) {
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

            updateState(new ChannelUID(missionGroupUID, CHANNEL_MISSION_CYCLE), cycle);
            updateState(new ChannelUID(missionGroupUID, CHANNEL_MISSION_PHASE), phase);
            updateState(new ChannelUID(missionGroupUID, CHANNEL_MISSION_ERROR),
                    String.valueOf(missionStatus.getError()));
            updateState(new ChannelUID(missionGroupUID, CHANNEL_MISSION_NUMBER), missionStatus.getnMssn());

            final BigInteger runtime = missionStatus.getMssnM();
            updateState(new ChannelUID(missionGroupUID, CHANNEL_MISSION_DURATION), new QuantityType<>(runtime, MINUTE));

            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            updateState(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_COMMAND), command);
        }

        final BigInteger language = reported.getLanguage();
        final List<Map<String, Number>> languages = reported.getLangs();
        if ((language != null) || ((languages != null) && !languages.isEmpty())) {
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            final ChannelUID languageChannelUID = new ChannelUID(controlGroupUID, CHANNEL_CONTROL_LANGUAGE);
            if (language != null) {
                if (channelContentProvider.isChannelPopulated(languageChannelUID)) {
                    updateState(languageChannelUID, language.toString());
                } else {
                    setCacheEntry(languageChannelUID, new StringType(language.toString()));
                }
            }

            if (languages != null) {
                if (!channelContentProvider.isChannelPopulated(languageChannelUID)) {
                    final Map<String, String> buffer = new HashMap<>();
                    for (final Map<String, Number> lang : languages) {
                        final String[] keys = lang.keySet().toArray(new String[lang.size()]);
                        buffer.put(String.valueOf(lang.get(keys[0])), keys[0]);
                    }
                    channelContentProvider.setLanguages(languageChannelUID, buffer);
                }
                final State state = getCacheEntry(languageChannelUID);
                if ((state != null) && channelContentProvider.isChannelPopulated(languageChannelUID)) {
                    updateState(languageChannelUID, state.toString());
                }
            }
        }

        final LastCommand lastCommand = reported.getLastCommand();
        if (lastCommand != null) {
            final ChannelGroupUID internalGroupUID = new ChannelGroupUID(thingUID, INTERNAL_GROUP_ID);
            updateState(new ChannelUID(internalGroupUID, CHANNEL_INTERNAL_LAST_COMMAND), lastCommand.toString());
        }

        final Boolean mapUploadAllowed = reported.getMapUploadAllowed();
        if (mapUploadAllowed != null) {
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            updateState(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_MAP_UPLOAD), OnOffType.from(mapUploadAllowed));
        }

        final String name = reported.getName();
        if (name != null) {
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, COMMON_GROUP_ID);
            updateState(new ChannelUID(controlGroupUID, CHANNEL_COMMON_NAME), name);
        }

        final NetInfo netinfo = reported.getNetinfo();
        if (netinfo != null) {
            final ChannelGroupUID networkGroupUID = new ChannelGroupUID(thingUID, NETWORK_GROUP_ID);
            final String bssid = netinfo.getBssid();
            if (bssid != null) {
                updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_BSSID), bssid.toUpperCase());
            }

            updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_DHCP), OnOffType.from(netinfo.getDhcp()));
            updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_SECURITY), netinfo.getSec());
        }

        final Boolean noAutoPasses = reported.getNoAutoPasses();
        final Boolean twoPasses = reported.getTwoPass();
        if ((noAutoPasses != null) || (twoPasses != null)) {
            // To make the life more interesting, paired values may not appear together in the
            // same message, so we have to keep track of current values.
            String state = null;
            final ChannelUID channelUID = new ChannelUID(thingUID, CONTROL_GROUP_ID, CHANNEL_CONTROL_CLEAN_PASSES);
            if (noAutoPasses != null) {
                state = Boolean.FALSE.equals(noAutoPasses) ? PASSES_AUTO : PASSES_1;
                setCacheEntry(channelUID, new StringType(state));
            }

            if (twoPasses != null) {
                // Can be overridden by "noAutoPasses":false
                final State cache = getCacheEntry(channelUID);
                if ((cache != null) && !PASSES_AUTO.equals(cache.toString())) {
                    state = Boolean.TRUE.equals(twoPasses) ? PASSES_2 : PASSES_1;
                }
            }

            updateState(channelUID, state);
        }

        final Pose pose = reported.getPose();
        if ((pose != null) && (pose.getPoint() != null)) {
            final ChannelGroupUID poseGroupUID = new ChannelGroupUID(thingUID, POSITION_GROUP_ID);
            updateState(new ChannelUID(poseGroupUID, CHANNEL_POSITION_X), pose.getPoint().getX());
            updateState(new ChannelUID(poseGroupUID, CHANNEL_POSITION_Y), pose.getPoint().getY());
            updateState(new ChannelUID(poseGroupUID, CHANNEL_POSITION_THETA), pose.getTheta());
            updateState(new ChannelUID(poseGroupUID, CHANNEL_JSON), gson.toJson(pose));
        }

        final Signal signal = reported.getSignal();
        if (signal != null) {
            final ChannelGroupUID networkGroupUID = new ChannelGroupUID(thingUID, NETWORK_GROUP_ID);
            updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_RSSI), signal.getRssi());
            updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_SNR), signal.getSnr());
        }

        final String timezone = reported.getTimezone();
        if (timezone != null) {
            final ChannelGroupUID commonGroupUID = new ChannelGroupUID(thingUID, COMMON_GROUP_ID);
            final ChannelUID timeZoneChannelUID = new ChannelUID(commonGroupUID, CHANNEL_COMMON_TIMEZONE);
            if (!channelContentProvider.isChannelPopulated(timeZoneChannelUID)) {
                channelContentProvider.setTimeZones(timeZoneChannelUID);
            }
            updateState(timeZoneChannelUID, timezone);
        }

        final WlanConfig wlanConfig = reported.getWlcfg();
        if (wlanConfig != null) {
            final ChannelGroupUID networkGroupUID = new ChannelGroupUID(thingUID, NETWORK_GROUP_ID);
            updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_SECURITY), wlanConfig.getSec());

            final String ssid = wlanConfig.getSsid();
            if (ssid != null) {
                String buffer = new String();
                for (int i = 0; i < ssid.length() / 2; i++) {
                    int hi = Character.digit(ssid.charAt(2 * i + 0), 16);
                    int lo = Character.digit(ssid.charAt(2 * i + 1), 16);
                    buffer = buffer + Character.toString(16 * hi + lo);
                }
                updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_SSID), buffer);
            }
        }

        updateProperty(PROPERTY_BATTERY_TYPE, reported.getBatteryType());
        updateProperty(PROPERTY_FIRMWARE_VERSION, reported.getSoftwareVer());
        updateProperty(PROPERTY_MODEL_ID, reported.getSku());
    }

    protected void sendSetting(final IRobotDTO setting) {
        connection.send("delta", String.format("{\"state\":%s}", gson.toJson(setting)));
    }

    protected void sendCommand(final String command, final @Nullable String arguments) {
        final String time = String.format("\"time\":%d", System.currentTimeMillis() / 1000);
        String request = String.format("\"command\":\"%s\", %s, \"initiator\":\"openhab\"", command, time);
        if ((arguments != null) && !arguments.isBlank()) {
            request = String.format("%s, %s", request, arguments);
        }
        connection.send("cmd", String.format("{ %s }", request));
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
}
