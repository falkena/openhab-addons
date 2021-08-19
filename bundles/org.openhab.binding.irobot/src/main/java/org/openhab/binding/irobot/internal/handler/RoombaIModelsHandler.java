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

import static org.openhab.binding.irobot.internal.IRobotBindingConstants.BINDING_ID;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_CONTROL_COMMAND;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_CONTROL_LANGUAGE;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_CONTROL_MAP_LEARN;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_NETWORK_ADDRESS;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_NETWORK_DNS1;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_NETWORK_DNS2;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_NETWORK_GATEWAY;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_NETWORK_MASK;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_NETWORK_NOISE;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_TYPE_NUMBER;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.COMMAND_CLEAN_REGIONS;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CONTROL_GROUP_ID;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.NETWORK_GROUP_ID;
import static org.openhab.core.thing.Thing.PROPERTY_HARDWARE_VERSION;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.binding.irobot.internal.IRobotChannelContentProvider;
import org.openhab.binding.irobot.internal.dto.DLangs;
import org.openhab.binding.irobot.internal.dto.HwPartsRev;
import org.openhab.binding.irobot.internal.dto.Languages;
import org.openhab.binding.irobot.internal.dto.NetInfo;
import org.openhab.binding.irobot.internal.dto.Reported;
import org.openhab.binding.irobot.internal.dto.Signal;
import org.openhab.binding.irobot.internal.dto.SubModSwVer;
import org.openhab.core.library.types.OnOffType;
import org.openhab.core.library.types.StringType;
import org.openhab.core.thing.ChannelGroupUID;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.builder.ChannelBuilder;
import org.openhab.core.thing.binding.builder.ThingBuilder;
import org.openhab.core.thing.type.AutoUpdatePolicy;
import org.openhab.core.thing.type.ChannelTypeUID;
import org.openhab.core.types.Command;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link RoombaIModelsHandler} is responsible for handling commands, which are
 * sent to one of the channels.
 *
 * @author hkuhn42 - Initial contribution
 * @author Pavel Fedin - Rewrite for 900 series
 * @author Florian Binder - added cleanRegions command and lastCommand channel
 * @author Alexander Falkenstern - Add support for I7 series
 */
@NonNullByDefault
public class RoombaIModelsHandler extends IRobotCommonHandler {
    private final Logger logger = LoggerFactory.getLogger(Roomba9ModelsHandler.class);

    private IRobotChannelContentProvider channelContentProvider;

    public RoombaIModelsHandler(Thing thing, IRobotChannelContentProvider channelContentProvider) {
        super(thing, channelContentProvider);
        this.channelContentProvider = channelContentProvider;
    }

    @Override
    public void initialize() {
        final ThingUID thingUID = thing.getUID();

        ThingBuilder tBuilder = editThing();
        ChannelUID channelUID = new ChannelUID(thingUID, CONTROL_GROUP_ID, CHANNEL_CONTROL_MAP_LEARN);
        if (thing.getChannel(channelUID) == null) {
            ChannelBuilder cBuilder = ChannelBuilder.create(channelUID, "Switch");
            cBuilder.withType(new ChannelTypeUID(BINDING_ID, CHANNEL_CONTROL_MAP_LEARN));
            cBuilder.withLabel("Learn maps");
            cBuilder.withDescription("Allow map learning");
            cBuilder.withAutoUpdatePolicy(AutoUpdatePolicy.VETO);
            tBuilder.withChannel(cBuilder.build());
        }

        channelUID = new ChannelUID(thingUID, NETWORK_GROUP_ID, CHANNEL_NETWORK_NOISE);
        if (thing.getChannel(channelUID) == null) {
            ChannelBuilder cBuilder = ChannelBuilder.create(channelUID, "Number");
            cBuilder.withType(new ChannelTypeUID(BINDING_ID, CHANNEL_TYPE_NUMBER));
            cBuilder.withLabel("Noise");
            cBuilder.withDescription("Wi-Fi signal noise");
            tBuilder.withChannel(cBuilder.build());
        }

        updateThing(tBuilder.build());
        super.initialize();
    }

    @Override
    public void handleCommand(ChannelUID channelUID, Command command) {
        final String channelId = channelUID.getIdWithoutGroup();
        if (command instanceof OnOffType) {
            if (CHANNEL_CONTROL_MAP_LEARN.equals(channelId)) {
                Reported request = new Reported();
                request.setPmapLearningAllowed(command.equals(OnOffType.ON));
                sendSetting(request);
            } else {
                super.handleCommand(channelUID, command);
            }
        } else if (command instanceof StringType) {
            if (CHANNEL_CONTROL_COMMAND.equals(channelId)) {
                String request = command.toString();
                if (request.startsWith(COMMAND_CLEAN_REGIONS)) {
                    // format: regions:<pmid>;<region_id1>,<region_id2>,...
                    if (Pattern.matches("regions:[^:;,]+;.+(,[^:;,]+)*", request)) {
                        final String[] regions = request.split(":");
                        final String[] params = regions[1].split(";");

                        String arguments = String.format("\"ordered\":1, \"pmap_id\":\"%s\", \"regions\":[", params[0]);
                        for (final String region : params[1].split(",")) {
                            arguments += String.format("{\"region_id\":\"%s\", \"type\":\"rid\"},", region);
                        }
                        sendCommand("start", arguments.replaceAll(",$", "") + "]");
                    } else {
                        logger.warn("Invalid request: {}", request);
                        logger.warn("Correct format: regions:<pmid>;<region_id1>,<region_id2>,...>");
                    }
                } else {
                    super.handleCommand(channelUID, command);
                }
            } else if (CHANNEL_CONTROL_LANGUAGE.equals(channelId)) {
                Languages languages = new Languages();
                languages.setsLang(command.toString());
                Reported request = new Reported();
                request.setLangs2(languages);
                sendSetting(request);
            } else {
                super.handleCommand(channelUID, command);
            }
        } else {
            super.handleCommand(channelUID, command);
        }
    }

    @Override
    protected void receive(final Reported reported) {
        final ThingUID thingUID = thing.getUID();

        final Languages languages = reported.getLangs2();
        if (languages != null) {
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            final ChannelUID languageChannelUID = new ChannelUID(controlGroupUID, CHANNEL_CONTROL_LANGUAGE);

            final DLangs langs = languages.getdLangs();
            if ((langs != null) && !channelContentProvider.isChannelPopulated(languageChannelUID)) {
                final Map<String, String> buffer = new HashMap<>();
                langs.getLangs().forEach(element -> buffer.put(element, element));
                channelContentProvider.setLanguages(languageChannelUID, buffer);
            }
            updateState(languageChannelUID, languages.getsLang());
        }

        final Boolean mapLearn = reported.getPmapLearningAllowed();
        if (mapLearn != null) {
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            updateState(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_MAP_LEARN), OnOffType.from(mapLearn));
        }

        final NetInfo netinfo = reported.getNetinfo();
        if (netinfo != null) {
            final ChannelGroupUID networkGroupUID = new ChannelGroupUID(thingUID, NETWORK_GROUP_ID);
            updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_ADDRESS), netinfo.getAddr());
            updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_DNS1), netinfo.getDns1());
            updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_DNS2), netinfo.getDns2());
            updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_GATEWAY), netinfo.getGw());
            updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_MASK), netinfo.getMask());
        }

        final Signal signal = reported.getSignal();
        if (signal != null) {
            final ChannelGroupUID networkGroupUID = new ChannelGroupUID(thingUID, NETWORK_GROUP_ID);
            updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_NOISE), signal.getNoise());
        }

        // "batInfo": {"mDate": "2019-3-12", "mName": "F12432832R", "mDaySerial": 34361, "mData":
        // "303030333034303200000000000000000000000000", "mLife":
        // "0C570B2210800A7A4E3900160507F6EE00D7FE9F2B0CFFFF005D320100000000", "cCount": 164, "afCount": 0}

        // "hwPartsRev": {
        // "csscID": 0,
        // "mobBlid": "XXXXXXXXXXXXXXXXXX",
        // "imuPartNo": "XXXXXXXXX",
        // "navSerialNo": "XXXXXXXXXX",
        // "wlan0HwAddr": "FF:FF:FF:FF:FF:FF",
        // "NavBrd": 0
        // }
        final HwPartsRev hardwareRevision = reported.getHwPartsRev();
        if (hardwareRevision != null) {
            updateProperty(PROPERTY_HARDWARE_VERSION, hardwareRevision.getMobBrd());
        }

        // "subModSwVer": {
        // "pwr": "0.5.5+ubuntu-HEAD-da83355e66c+21",
        // "sft": "1.2.0+Lewis-Builds/Lewis-Certified-Safety/lewis-safety-ca6f27d09c6+31",
        // "linux": "linux+3.8.6.1+lewis-release-121+21"
        // }
        final SubModSwVer softwareVersion = reported.getSubModSwVer();
        if (softwareVersion != null) {
            updateProperty("bootloaderVersion", softwareVersion.getMobBtl());
            updateProperty("mobilityVersion", softwareVersion.getMob());
            updateProperty("navigationSoftwareVersion", softwareVersion.getNav());
            updateProperty("wifiSoftwareVersion", softwareVersion.getCon());
        }

        super.receive(reported);
    }
}
