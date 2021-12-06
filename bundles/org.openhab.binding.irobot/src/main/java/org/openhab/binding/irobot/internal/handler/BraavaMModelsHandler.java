/**
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
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_TYPE_NUMBER;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CONTROL_GROUP_ID;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.NETWORK_GROUP_ID;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.binding.irobot.internal.IRobotChannelContentProvider;
import org.openhab.binding.irobot.internal.dto.IRobotDTO;
import org.openhab.binding.irobot.internal.dto.Langs2;
import org.openhab.binding.irobot.internal.dto.Languages2;
import org.openhab.binding.irobot.internal.dto.Reported;
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
 * The {@link BraavaMModelsHandler} is responsible for handling commands, which are
 * sent to one of the channels.
 *
 * @author Alexander Falkenstern - Introduce handler for M-series mopping robots
 */
@NonNullByDefault
public class BraavaMModelsHandler extends IRobotCommonHandler {
    private final Logger logger = LoggerFactory.getLogger(BraavaMModelsHandler.class);

    private IRobotChannelContentProvider channelContentProvider;

    public BraavaMModelsHandler(Thing thing, IRobotChannelContentProvider channelContentProvider) {
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

        super.receive(reported);
    }
}
