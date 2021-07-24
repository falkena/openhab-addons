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
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_SCHEDULE_ENABLED;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_SCHEDULE_TIMESTAMP;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CONTROL_GROUP_ID;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.DAY_OF_WEEK;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.SCHEDULE_GROUP_ID;
import static org.openhab.core.thing.Thing.PROPERTY_HARDWARE_VERSION;

import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.List;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.binding.irobot.internal.dto.CleanSchedule;
import org.openhab.binding.irobot.internal.dto.Reported;
import org.openhab.core.library.types.DateTimeType;
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
import org.openhab.core.types.State;
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
        if (command instanceof DateTimeType) {
            if (SCHEDULE_GROUP_ID.equals(channelUID.getGroupId())) {
                CleanSchedule schedule = new CleanSchedule();
                List<String> cycle = schedule.getCycle();
                List<BigInteger> hour = schedule.getH();
                List<BigInteger> minute = schedule.getM();
                final ChannelGroupUID groupUID = new ChannelGroupUID(channelUID.getThingUID(), SCHEDULE_GROUP_ID);
                for (int i = 0; i < DAY_OF_WEEK.length; i++) {
                    final ChannelUID enabledChannelUID = new ChannelUID(groupUID, CHANNEL_SCHEDULE_ENABLED[i]);
                    cycle.add(OnOffType.ON.equals(getCacheEntry(enabledChannelUID)) ? "start" : "none");

                    final ChannelUID timeChannelUID = new ChannelUID(groupUID, CHANNEL_SCHEDULE_TIMESTAMP[i]);
                    DateTimeType timestamp = (DateTimeType) getCacheEntry(timeChannelUID);
                    if ((timestamp == null) || channelId.equals(CHANNEL_SCHEDULE_TIMESTAMP[i])) {
                        timestamp = (DateTimeType) command;
                    }
                    hour.add(BigInteger.valueOf(timestamp.getZonedDateTime().getHour()));
                    minute.add(BigInteger.valueOf(timestamp.getZonedDateTime().getMinute()));
                }

                Reported request = new Reported();
                request.setCleanSchedule(schedule);
                sendSetting(request);
            } else {
                super.handleCommand(channelUID, command);
            }
        } else if (command instanceof OnOffType) {
            if (SCHEDULE_GROUP_ID.equals(channelUID.getGroupId())) {
                CleanSchedule schedule = new CleanSchedule();
                List<String> cycle = schedule.getCycle();
                List<BigInteger> hour = schedule.getH();
                List<BigInteger> minute = schedule.getM();
                final ChannelGroupUID groupUID = new ChannelGroupUID(channelUID.getThingUID(), SCHEDULE_GROUP_ID);
                for (int i = 0; i < DAY_OF_WEEK.length; i++) {
                    final ChannelUID enabledChannelUID = new ChannelUID(groupUID, CHANNEL_SCHEDULE_ENABLED[i]);
                    OnOffType enabled = (OnOffType) getCacheEntry(enabledChannelUID);
                    if ((enabled == null) || channelId.equals(CHANNEL_SCHEDULE_ENABLED[i])) {
                        enabled = (OnOffType) command;
                    }
                    cycle.add(OnOffType.ON.equals(enabled) ? "start" : "none");

                    final ChannelUID timeChannelUID = new ChannelUID(groupUID, CHANNEL_SCHEDULE_TIMESTAMP[i]);
                    DateTimeType timestamp = (DateTimeType) getCacheEntry(timeChannelUID);
                    hour.add(BigInteger.valueOf(timestamp.getZonedDateTime().getHour()));
                    minute.add(BigInteger.valueOf(timestamp.getZonedDateTime().getMinute()));
                }
                Reported request = new Reported();
                request.setCleanSchedule(schedule);
                sendSetting(request);
            } else if (CHANNEL_CONTROL_EDGE_CLEAN.equals(channelId)) {
                Reported request = new Reported();
                request.setOpenOnly(command.equals(OnOffType.OFF));
                sendSetting(request);
            } else {
                super.handleCommand(channelUID, command);
            }
        } else if (command instanceof StringType) {
            if (CHANNEL_CONTROL_POWER_BOOST.equals(channelId)) {
                Reported request = new Reported();
                request.setCarpetBoost(command.equals(BOOST_AUTO));
                request.setVacHigh(command.equals(BOOST_PERFORMANCE));
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

        final CleanSchedule cleanSchedule = reported.getCleanSchedule();
        if (cleanSchedule != null) {
            final List<String> cycles = cleanSchedule.getCycle();
            final List<BigInteger> hours = cleanSchedule.getH();
            final List<BigInteger> minutes = cleanSchedule.getM();
            if ((cycles != null) && (hours != null) && (minutes != null)) {
                final LocalDate today = LocalDate.now();
                final ChannelGroupUID scheduleGroupUID = new ChannelGroupUID(thingUID, SCHEDULE_GROUP_ID);
                for (int i = 0; i < DAY_OF_WEEK.length; i++) {
                    final OnOffType enabled = OnOffType.from("start".equalsIgnoreCase(cycles.get(i)));
                    updateState(new ChannelUID(scheduleGroupUID, CHANNEL_SCHEDULE_ENABLED[i]), enabled);
                    final LocalTime time = LocalTime.of(hours.get(i).intValue(), minutes.get(i).intValue());
                    DateTimeType timestamp = new DateTimeType(ZonedDateTime.of(today, time, ZoneId.systemDefault()));
                    updateState(new ChannelUID(scheduleGroupUID, CHANNEL_SCHEDULE_TIMESTAMP[i]), timestamp);
                }
            }
        }

        final Boolean boost = reported.getCarpetBoost();
        final Boolean vacHigh = reported.getVacHigh();
        if ((boost != null) || (vacHigh != null)) {
            // To make the life more interesting, paired values may not appear together in the
            // same message, so we have to keep track of current values.
            String state = null;
            final ChannelUID channelUID = new ChannelUID(thingUID, CONTROL_GROUP_ID, CHANNEL_CONTROL_POWER_BOOST);

            if (boost != null) {
                state = Boolean.TRUE.equals(boost) ? BOOST_AUTO : BOOST_ECO;
                setCacheEntry(channelUID, new StringType(state));
            }

            if (vacHigh != null) {
                // Can be overridden by "carpetBoost":true
                final State cache = getCacheEntry(channelUID);
                if ((cache != null) && !cache.equals(BOOST_AUTO)) {
                    state = Boolean.TRUE.equals(vacHigh) ? BOOST_PERFORMANCE : BOOST_ECO;
                }
            }

            updateState(channelUID, state);
        }

        final Boolean openOnly = reported.getOpenOnly();
        if (openOnly != null) {
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            updateState(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_EDGE_CLEAN), OnOffType.from(!openOnly));
        }

        updateProperty("bootloaderVersion", reported.getBootloaderVer());
        updateProperty(PROPERTY_HARDWARE_VERSION, reported.getHardwareRev());
        updateProperty("mobilityVersion", reported.getMobilityVer());
        updateProperty("navigationSoftwareVersion", reported.getNavSwVer());
        updateProperty("soundVer", reported.getSoundVer());
        updateProperty("umiVer", reported.getUmiVer());
        updateProperty("uiSwVer", reported.getUiSwVer());
        updateProperty("wifiSoftwareVersion", reported.getWifiSwVer());

        super.receive(reported);
    }
}
