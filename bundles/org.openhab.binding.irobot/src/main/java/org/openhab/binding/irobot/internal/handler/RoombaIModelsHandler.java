/**
 * Copyright (c) 2010-2021 Contributors to the openHAB project
 * Copyright (c) 2021 Alexander Falkenstern
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
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_CONTROL_LANGUAGE;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_CONTROL_MAP_LEARN;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_NETWORK_NOISE;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_NETWORK_RSSI;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_NETWORK_SNR;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_TYPE_NUMBER;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CONTROL_GROUP_ID;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.NETWORK_GROUP_ID;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.binding.irobot.internal.IRobotChannelContentProvider;
import org.openhab.binding.irobot.internal.dto.IRobotDTO;
import org.openhab.binding.irobot.internal.dto.Langs2;
import org.openhab.binding.irobot.internal.dto.Languages2;
import org.openhab.binding.irobot.internal.dto.MapLearning;
import org.openhab.binding.irobot.internal.dto.Reported;
import org.openhab.binding.irobot.internal.dto.Signal;
import org.openhab.core.library.types.DecimalType;
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
import org.openhab.core.types.RefreshType;
import org.openhab.core.types.UnDefType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link RoombaIModelsHandler} is responsible for handling commands, which are
 * sent to one of the channels.
 *
 * @author Alexander Falkenstern - Introduce handler for I-series vacuum robots
 */
@NonNullByDefault
public class RoombaIModelsHandler extends RoombaCommonHandler {
    private final Logger logger = LoggerFactory.getLogger(RoombaIModelsHandler.class);

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
        if (command instanceof RefreshType) {
            final IRobotDTO cache = getCacheEntry(channelUID);
            if (cache instanceof Langs2) {
                final Langs2 languages2 = (Langs2) cache;
                if (CHANNEL_CONTROL_LANGUAGE.equals(channelId)) {
                    if (channelContentProvider.isChannelPopulated(channelUID)) {
                        updateState(channelUID, languages2.getsLang());
                    } else {
                        updateState(channelUID, UnDefType.UNDEF);
                    }
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for map upload values.", channelUID);
                }
            } else if (cache instanceof MapLearning) {
                final MapLearning mapLearning = (MapLearning) cache;
                if (CHANNEL_CONTROL_MAP_LEARN.equals(channelId)) {
                    final Boolean learn = mapLearning.getPmapLearningAllowed();
                    updateState(channelUID, learn != null ? OnOffType.from(learn) : UnDefType.UNDEF);
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for map upload values.", channelUID);
                }
            } else if (cache instanceof Signal) {
                final Signal signal = (Signal) cache;
                if (CHANNEL_NETWORK_NOISE.equals(channelId)) {
                    final BigInteger noise = signal.getNoise();
                    updateState(channelUID, noise != null ? new DecimalType(noise.longValue()) : UnDefType.UNDEF);
                } else if (CHANNEL_NETWORK_RSSI.equals(channelId) || (CHANNEL_NETWORK_SNR.equals(channelId))) {
                    super.handleCommand(channelUID, command);
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for signal values.", channelUID);
                }
            } else {
                super.handleCommand(channelUID, command);
            }
        } else if (command instanceof OnOffType) {
            if (CHANNEL_CONTROL_MAP_LEARN.equals(channelId)) {
                sendSetting(new MapLearning(command.equals(OnOffType.ON)));
            } else {
                super.handleCommand(channelUID, command);
            }
        } else if (command instanceof StringType) {
            if (CHANNEL_CONTROL_LANGUAGE.equals(channelId)) {
                sendSetting(new Languages2(new Langs2(null, null, command.toString(), null)));
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

        Langs2 languages2 = reported.getLangs2();
        if (languages2 != null) {
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            final ChannelUID languageChannelUID = new ChannelUID(controlGroupUID, CHANNEL_CONTROL_LANGUAGE);
            if (!channelContentProvider.isChannelPopulated(languageChannelUID)) {
                final Map<String, String> buffer = new HashMap<>();
                for (final String lang : languages2.getdLangs().getLangs()) {
                    buffer.put(lang, lang);
                }
                channelContentProvider.setLanguages(languageChannelUID, buffer);
            }
            setCacheEntry(languageChannelUID, languages2);
        }

        final Boolean mapLearning = reported.getPmapLearningAllowed();
        if (mapLearning != null) {
            final MapLearning learn = new MapLearning(mapLearning);
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            setCacheEntry(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_MAP_LEARN), learn);
        }

        final Signal signal = reported.getSignal();
        if (signal != null) {
            final ChannelGroupUID networkGroupUID = new ChannelGroupUID(thingUID, NETWORK_GROUP_ID);
            setCacheEntry(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_NOISE), signal);
        }
        /*
         * if (reported.subModSwVer != null) {
         * // This is used by i7 model. It has more capabilities, perhaps a dedicated
         * // handler should be written by someone who owns it.
         * reportProperty("subModSwVer.nav", reported.subModSwVer.nav);
         * reportProperty("subModSwVer.mob", reported.subModSwVer.mob);
         * reportProperty("subModSwVer.pwr", reported.subModSwVer.pwr);
         * reportProperty("subModSwVer.sft", reported.subModSwVer.sft);
         * reportProperty("subModSwVer.mobBtl", reported.subModSwVer.mobBtl);
         * reportProperty("subModSwVer.linux", reported.subModSwVer.linux);
         * reportProperty("subModSwVer.con", reported.subModSwVer.con);
         * }
         */
        // "batInfo": {"mDate": "2019-3-12", "mName": "F12432832R", "mDaySerial": 34361, "mData":
        // "303030333034303200000000000000000000000000", "mLife":
        // "0C570B2210800A7A4E3900160507F6EE00D7FE9F2B0CFFFF005D320100000000", "cCount": 164, "afCount": 0}
        // "subModSwVer": {"nav": "lewis-nav+3.14.16+ubuntu-HEAD-da83355e66c+21", "mob":
        // "3.14.16+ubuntu-HEAD-da83355e66c+21", "pwr": "0.5.5+ubuntu-HEAD-da83355e66c+21", "sft":
        // "1.2.0+Lewis-Builds/Lewis-Certified-Safety/lewis-safety-ca6f27d09c6+31", "mobBtl": "4.2", "linux":
        // "linux+3.8.6.1+lewis-release-121+21", "con": "3.9.6.1-tags/release-3.9.6.1@ffb83460/ubuntu"}
        // "hwPartsRev": {"csscID": 0, "mobBrd": 7, "mobBlid": "7DD8D0D52D07D61CE33AE4737614926C", "imuPartNo":
        // "BMI055", "navSerialNo": "CF0960ZS0", "wlan0HwAddr": "50:14:79:13:f9:b7", "NavBrd": 0}
        super.receive(reported);
    }
}
