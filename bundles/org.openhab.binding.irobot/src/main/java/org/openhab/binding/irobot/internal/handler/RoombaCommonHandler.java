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

import java.math.BigDecimal;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.binding.irobot.internal.IRobotChannelContentProvider;
import org.openhab.binding.irobot.internal.utils.JSONUtils;
import org.openhab.binding.irobot.internal.utils.Requests;
import org.openhab.core.library.types.OnOffType;
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
 * The {@link RoombaCommonHandler} is responsible for handling common JSON replies.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class RoombaCommonHandler extends IRobotCommonHandler {
    private final Logger logger = LoggerFactory.getLogger(RoombaCommonHandler.class);

    public RoombaCommonHandler(Thing thing, IRobotChannelContentProvider channelContentProvider) {
        super(thing, channelContentProvider);
    }

    @Override
    public void initialize() {
        ThingBuilder tBuilder = editThing();
        final ThingUID thingUID = thing.getUID();

        ChannelUID channelUID = new ChannelUID(thingUID, STATE_GROUP_ID, CHANNEL_STATE_BIN);
        if (thing.getChannel(channelUID) == null) {
            ChannelBuilder cBuilder = ChannelBuilder.create(channelUID, "String");
            cBuilder.withType(new ChannelTypeUID(BINDING_ID, "bin"));
            cBuilder.withLabel("Bin status");
            cBuilder.withDescription("Robot bin status");
            tBuilder.withChannel(cBuilder.build());
        }

        channelUID = new ChannelUID(thingUID, COMMON_GROUP_ID, CHANNEL_COMMON_SCRUBS);
        if (thing.getChannel(channelUID) == null) {
            ChannelBuilder cBuilder = ChannelBuilder.create(channelUID, "Number");
            cBuilder.withType(new ChannelTypeUID(BINDING_ID, CHANNEL_TYPE_NUMBER));
            cBuilder.withLabel("Scrub count");
            cBuilder.withDescription("Robot lifetime scrub count");
            tBuilder.withChannel(cBuilder.build());
        }

        updateThing(tBuilder.build());
        super.initialize();
    }

    @Override
    public void handleCommand(ChannelUID channelUID, Command command) {
        final String channelId = channelUID.getIdWithoutGroup();

        if (command instanceof OnOffType) {
            final JsonObject request = new JsonObject();
            if (CHANNEL_CONTROL_ALWAYS_FINISH.equals(channelId)) {
                // Binding operate with inverse of "binPause"
                request.addProperty("binPause", command.equals(OnOffType.OFF));
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

        final Boolean binPause = JSONUtils.getAsBoolean("binPause", tree);
        final Boolean continueVacuum = (binPause != null) ? !binPause : null;
        updateState(new ChannelUID(thingUID, CONTROL_GROUP_ID, CHANNEL_CONTROL_ALWAYS_FINISH), continueVacuum);

        final JsonElement bin = JSONUtils.find("bin", tree);
        if (bin != null) {
            // The bin cannot be both full and removed simultaneously, so let's encode it as a single value
            String status = null;
            if (Boolean.FALSE.equals(JSONUtils.getAsBoolean("present", bin))) {
                status = STATE_BIN_REMOVED;
            } else {
                final Boolean full = JSONUtils.getAsBoolean("full", bin);
                status = Boolean.TRUE.equals(full) ? STATE_BIN_FULL : STATE_BIN_OK;
            }

            updateState(new ChannelUID(thingUID, STATE_GROUP_ID, CHANNEL_STATE_BIN), status);
        }

        final JsonElement run = JSONUtils.find("bbrun", tree);
        if (run != null) {
            final BigDecimal scrubs = JSONUtils.getAsDecimal("nScrubs", run);
            updateState(new ChannelUID(thingUID, COMMON_GROUP_ID, CHANNEL_COMMON_SCRUBS), scrubs);
        }

        super.receive(topic, tree);
    }
}
