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
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.BOOST_AUTO;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.BOOST_ECO;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.BOOST_PERFORMANCE;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_CONTROL_EDGE_CLEAN;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_CONTROL_POWER_BOOST;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CONTROL_GROUP_ID;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.DAY_OF_WEEK;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.SCHEDULE_GROUP_ID;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.irobot.internal.dto.MQTTProtocol;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link Roomba9ModelsHandler} is responsible for handling commands, which are
 * sent to one of the channels.
 *
 * @author hkuhn42 - Initial contribution
 * @author Pavel Fedin - Rewrite for 900 series
 * @author Florian Binder - added cleanRegions command and lastCommand channel
 * @author Alexander Falkenstern - Add support for I7 series
 */
@NonNullByDefault
public class Roomba9ModelsHandler extends IRobotCommonHandler {
    private final Logger logger = LoggerFactory.getLogger(Roomba9ModelsHandler.class);

    private boolean carpetBoost = true;
    private @Nullable Boolean vacHigh = null;
    private MQTTProtocol.@Nullable Schedule lastSchedule = null;

    public Roomba9ModelsHandler(Thing thing) {
        super(thing);
    }

    @Override
    public void initialize() {
        final ThingUID thingUID = thing.getUID();

        ThingBuilder tBuilder = editThing();
        ChannelUID channelUID = new ChannelUID(thingUID, CONTROL_GROUP_ID, CHANNEL_CONTROL_EDGE_CLEAN);
        if (thing.getChannel(channelUID) == null) {
            ChannelBuilder cBuilder = ChannelBuilder.create(channelUID, "Switch");
            cBuilder.withType(new ChannelTypeUID(BINDING_ID, CHANNEL_CONTROL_EDGE_CLEAN));
            cBuilder.withLabel("Edge clean");
            cBuilder.withDescription("Seek out and clean along walls and furniture legs");
            cBuilder.withAutoUpdatePolicy(AutoUpdatePolicy.VETO);
            tBuilder.withChannel(cBuilder.build());
        }

        channelUID = new ChannelUID(thingUID, CONTROL_GROUP_ID, CHANNEL_CONTROL_POWER_BOOST);
        if (thing.getChannel(channelUID) == null) {
            ChannelBuilder cBuilder = ChannelBuilder.create(channelUID, "String");
            cBuilder.withType(new ChannelTypeUID(BINDING_ID, CHANNEL_CONTROL_POWER_BOOST));
            cBuilder.withLabel("Power boost");
            cBuilder.withDescription("Carpet boost mode");
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
            super.handleCommand(channelUID, command);
        } else if (command instanceof OnOffType) {
            if (SCHEDULE_GROUP_ID.equals(channelUID.getGroupId())) {
                MQTTProtocol.Schedule schedule = lastSchedule;

                // Schedule can only be updated in a bulk, so we have to store current schedule and modify components.
                if ((schedule != null) && (schedule.cycle != null)) {
                    MQTTProtocol.Schedule newSchedule = new MQTTProtocol.Schedule(schedule.cycle);
                    for (int i = 0; i < DAY_OF_WEEK.length; i++) {
                        newSchedule.enableCycle(i, newSchedule.cycleEnabled(i));
                        if (channelUID.getIdWithoutGroup().equals(DAY_OF_WEEK[i] + "_enabled")) {
                            newSchedule.enableCycle(i, command.equals(OnOffType.ON));
                        }
                    }
                    sendDelta(new MQTTProtocol.CleanSchedule(newSchedule));
                }
            } else if (CHANNEL_CONTROL_EDGE_CLEAN.equals(channelId)) {
                sendDelta(new MQTTProtocol.OpenOnly(command.equals(OnOffType.OFF)));
            } else {
                super.handleCommand(channelUID, command);
            }
        } else if (command instanceof StringType) {
            if (CHANNEL_CONTROL_POWER_BOOST.equals(channelId)) {
                sendDelta(new MQTTProtocol.PowerBoost(command.equals(BOOST_AUTO), command.equals(BOOST_PERFORMANCE)));
            } else {
                super.handleCommand(channelUID, command);
            }
        } else {
            super.handleCommand(channelUID, command);
        }
    }

    @Override
    public void receive(final String topic, final MQTTProtocol.GenericState reported) {
        final ThingUID thingUID = thing.getUID();

        if (reported.cleanSchedule != null) {
            final ChannelGroupUID scheduleGroupUID = new ChannelGroupUID(thingUID, SCHEDULE_GROUP_ID);
            MQTTProtocol.Schedule schedule = reported.cleanSchedule;

            if (schedule.cycle != null) {
                for (int i = 0; i < DAY_OF_WEEK.length; i++) {
                    updateState(new ChannelUID(scheduleGroupUID, DAY_OF_WEEK[i] + "_enabled"),
                            schedule.cycleEnabled(i));
                }
            }

            lastSchedule = schedule;
        }

        if (reported.openOnly != null) {
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            updateState(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_EDGE_CLEAN), !reported.openOnly);
        }

        // To make the life more interesting, paired values may not appear together in the same message,
        // so we have to keep track of current values.
        if (reported.carpetBoost != null) {
            carpetBoost = reported.carpetBoost;
            if (reported.carpetBoost) {
                // When set to true, overrides vacHigh
                final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
                updateState(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_POWER_BOOST), BOOST_AUTO);
            } else if (vacHigh != null) {
                reportVacHigh();
            }
        }

        if (reported.vacHigh != null) {
            vacHigh = reported.vacHigh;
            if (!carpetBoost) {
                // Can be overridden by "carpetBoost":true
                reportVacHigh();
            }
        }

        updateProperty(Thing.PROPERTY_FIRMWARE_VERSION, reported.softwareVer);
        updateProperty("navSwVer", reported.navSwVer);
        updateProperty("wifiSwVer", reported.wifiSwVer);
        updateProperty("mobilityVer", reported.mobilityVer);
        updateProperty("bootloaderVer", reported.bootloaderVer);
        updateProperty("umiVer", reported.umiVer);
        updateProperty("sku", reported.sku);
        updateProperty("batteryType", reported.batteryType);

        super.receive(topic, reported);
    }

    private void reportVacHigh() {
        final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thing.getUID(), CONTROL_GROUP_ID);
        updateState(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_POWER_BOOST),
                vacHigh ? BOOST_PERFORMANCE : BOOST_ECO);
    }
}
