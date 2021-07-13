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
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_CONTROL_MAP_LEARN;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_NETWORK_NOISE;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_TYPE_NUMBER;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CONTROL_GROUP_ID;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.NETWORK_GROUP_ID;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.binding.irobot.internal.dto.MQTTProtocol;
import org.openhab.core.library.types.OnOffType;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.builder.ChannelBuilder;
import org.openhab.core.thing.binding.builder.ThingBuilder;
import org.openhab.core.thing.type.AutoUpdatePolicy;
import org.openhab.core.thing.type.ChannelTypeUID;
import org.openhab.core.types.Command;
import org.openhab.core.types.RefreshType;
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

    public RoombaIModelsHandler(Thing thing) {
        super(thing);
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
            super.handleCommand(channelUID, command);
        } else if (command instanceof OnOffType) {
            super.handleCommand(channelUID, command);
        } else {
            super.handleCommand(channelUID, command);
        }
    }

    @Override
    public void receive(final String topic, final MQTTProtocol.GenericState reported) {
        final ThingUID thingUID = thing.getUID();

        if (reported.subModSwVer != null) {
            // This is used by i7 model. It has more capabilities, perhaps a dedicated
            // handler should be written by someone who owns it.
            updateProperty("subModSwVer.nav", reported.subModSwVer.nav);
            updateProperty("subModSwVer.mob", reported.subModSwVer.mob);
            updateProperty("subModSwVer.pwr", reported.subModSwVer.pwr);
            updateProperty("subModSwVer.sft", reported.subModSwVer.sft);
            updateProperty("subModSwVer.mobBtl", reported.subModSwVer.mobBtl);
            updateProperty("subModSwVer.linux", reported.subModSwVer.linux);
            updateProperty("subModSwVer.con", reported.subModSwVer.con);
        }

        super.receive(topic, reported);
    }
}
