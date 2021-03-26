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
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_TYPE_NUMBER;

import java.math.BigDecimal;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.binding.irobot.internal.IRobotChannelContentProvider;
import org.openhab.binding.irobot.internal.utils.JSONUtils;
import org.openhab.binding.irobot.internal.utils.Requests;
import org.openhab.core.library.types.OnOffType;
import org.openhab.core.library.types.StringType;
import org.openhab.core.thing.ChannelGroupUID;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.builder.ChannelBuilder;
import org.openhab.core.thing.binding.builder.ThingBuilder;
import org.openhab.core.thing.type.ChannelTypeUID;
import org.openhab.core.types.Command;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

/**
 * The {@link BraavaCommonHandler} is responsible for handling common JSON replies.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class BraavaCommonHandler extends IRobotCommonHandler {
    private final Logger logger = LoggerFactory.getLogger(BraavaCommonHandler.class);

    public BraavaCommonHandler(Thing thing, IRobotChannelContentProvider channelContentProvider) {
        super(thing, channelContentProvider);
    }

    @Override
    public void initialize() {
        ThingBuilder tBuilder = editThing();
        final ThingUID thingUID = thing.getUID();

        ChannelUID channelUID = new ChannelUID(thingUID, CONTROL_GROUP_ID, CHANNEL_CONTROL_MAP_LEARN);
        if (thing.getChannel(channelUID) == null) {
            ChannelBuilder cBuilder = ChannelBuilder.create(channelUID, "Switch");
            cBuilder.withType(new ChannelTypeUID(BINDING_ID, CHANNEL_CONTROL_MAP_LEARN));
            cBuilder.withLabel("Learn maps");
            cBuilder.withDescription("Allow map learning");
            tBuilder.withChannel(cBuilder.build());
        }

        channelUID = new ChannelUID(thingUID, CONTROL_GROUP_ID, CHANNEL_CONTROL_POWER_BOOST);
        if (thing.getChannel(channelUID) == null) {
            ChannelBuilder cBuilder = ChannelBuilder.create(channelUID, "String");
            cBuilder.withType(new ChannelTypeUID(BINDING_ID, CHANNEL_CONTROL_POWER_BOOST));
            cBuilder.withLabel("Power boost");
            cBuilder.withDescription("Carpet boost mode");
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
            final JsonObject state = new JsonObject();
            if (CHANNEL_CONTROL_MAP_LEARN.equals(channelId)) {
                state.addProperty("pmapLearningAllowed", command.equals(OnOffType.ON));
                connection.send(new Requests.DeltaRequest(state));
            } else {
                super.handleCommand(channelUID, command);
            }
        } else if (command instanceof StringType) {
            if (CHANNEL_CONTROL_POWER_BOOST.equals(channelId)) {
                final JsonObject request = new JsonObject();
                request.addProperty("carpetBoost", command.equals(BOOST_AUTO));
                request.addProperty("vacHigh", command.equals(BOOST_PERFORMANCE));
                connection.send(new Requests.DeltaRequest(request));
            } else {
                super.handleCommand(channelUID, command);
            }
        } else {
            super.handleCommand(channelUID, command);
        }
    }

    @Override
    public void receive(final String topic, final JsonElement tree) {
        final ThingUID thingUID = thing.getUID();

        final Boolean allowLearning = JSONUtils.getAsBoolean("pmapLearningAllowed", tree);
        updateState(new ChannelUID(thingUID, CONTROL_GROUP_ID, CHANNEL_CONTROL_MAP_LEARN), allowLearning);

        final ChannelGroupUID networkGroupUID = new ChannelGroupUID(thingUID, NETWORK_GROUP_ID);
        final String mac = JSONUtils.getAsString("wlan0HwAddr", tree);
        if (mac != null) {
            updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_MAC), mac.toUpperCase());
        }

        final BigDecimal noise = JSONUtils.getAsDecimal("noise", tree);
        updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_NOISE), noise);

        final String address = JSONUtils.getAsString("addr", tree);
        updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_ADDRESS), address);

        final String mask = JSONUtils.getAsString("mask", tree);
        updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_MASK), mask);

        final String gateway = JSONUtils.getAsString("gw", tree);
        updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_GATEWAY), gateway);

        final String dns1 = JSONUtils.getAsString("dns1", tree);
        updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_DNS1), dns1);

        final String dns2 = JSONUtils.getAsString("dns2", tree);
        updateState(new ChannelUID(networkGroupUID, CHANNEL_NETWORK_DNS2), dns2);

        super.receive(topic, tree);
    }
}
