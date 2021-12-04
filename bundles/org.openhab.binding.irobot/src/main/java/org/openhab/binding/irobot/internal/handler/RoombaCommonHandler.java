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
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_CONTROL_ALWAYS_FINISH;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_STATE_BIN;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CONTROL_GROUP_ID;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.STATE_BIN_FULL;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.STATE_BIN_OK;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.STATE_BIN_REMOVED;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.STATE_GROUP_ID;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.binding.irobot.internal.dto.BinPause;
import org.openhab.binding.irobot.internal.dto.BinStatus;
import org.openhab.binding.irobot.internal.dto.IRobotDTO;
import org.openhab.binding.irobot.internal.dto.Reported;
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
 * The {@link RoombaCommonHandler} is responsible for handling commands, which are
 * sent to one of the channels.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class RoombaCommonHandler extends IRobotCommonHandler {
    private final Logger logger = LoggerFactory.getLogger(RoombaCommonHandler.class);

    public RoombaCommonHandler(Thing thing) {
        super(thing);
    }

    @Override
    public void initialize() {
        final ThingUID thingUID = thing.getUID();

        ThingBuilder tBuilder = editThing();
        ChannelUID channelUID = new ChannelUID(thingUID, CONTROL_GROUP_ID, CHANNEL_CONTROL_ALWAYS_FINISH);
        if (thing.getChannel(channelUID) == null) {
            ChannelBuilder cBuilder = ChannelBuilder.create(channelUID, "Switch");
            cBuilder.withType(new ChannelTypeUID(BINDING_ID, CHANNEL_CONTROL_ALWAYS_FINISH));
            cBuilder.withAutoUpdatePolicy(AutoUpdatePolicy.VETO);
            tBuilder.withChannel(cBuilder.build());
        }

        channelUID = new ChannelUID(thingUID, STATE_GROUP_ID, CHANNEL_STATE_BIN);
        if (thing.getChannel(channelUID) == null) {
            ChannelBuilder cBuilder = ChannelBuilder.create(channelUID, "String");
            cBuilder.withType(new ChannelTypeUID(BINDING_ID, CHANNEL_STATE_BIN));
            cBuilder.withAutoUpdatePolicy(AutoUpdatePolicy.VETO);
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
            if (cache instanceof BinPause) {
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
            } else {
                updateState(channelUID, UnDefType.UNDEF);
            }
        } else if (command instanceof OnOffType) {
            if (CHANNEL_CONTROL_ALWAYS_FINISH.equals(channelId)) {
                sendSetting(new BinPause(command.equals(OnOffType.OFF)));
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

        final Boolean binPause = reported.getBinPause();
        if (binPause != null) {
            final BinPause pause = new BinPause(binPause);
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            setCacheEntry(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_ALWAYS_FINISH), pause);
        }

        final BinStatus binStatus = reported.getBin();
        if (binStatus != null) {
            final ChannelGroupUID stateGroupUID = new ChannelGroupUID(thingUID, STATE_GROUP_ID);
            setCacheEntry(new ChannelUID(stateGroupUID, CHANNEL_STATE_BIN), binStatus);
        }

        super.receive(reported);
    }
}
