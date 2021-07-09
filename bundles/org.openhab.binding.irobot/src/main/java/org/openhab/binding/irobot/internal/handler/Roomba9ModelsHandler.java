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
import static org.openhab.core.thing.Thing.PROPERTY_HARDWARE_VERSION;

import java.math.BigInteger;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.binding.irobot.internal.dto.CleanSchedule;
import org.openhab.binding.irobot.internal.dto.IRobotDTO;
import org.openhab.binding.irobot.internal.dto.OpenOnly;
import org.openhab.binding.irobot.internal.dto.PowerBoost;
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
import org.openhab.core.types.RefreshType;
import org.openhab.core.types.UnDefType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link Roomba9ModelsHandler} is responsible for handling commands, which are
 * sent to one of the channels.
 *
 * @author Alexander Falkenstern - Introduce handle for 9-series vacuum robots
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
        if (command instanceof RefreshType) {
            final IRobotDTO cache = getCacheEntry(channelUID);
            if (cache instanceof CleanSchedule) {
                final CleanSchedule schedule = (CleanSchedule) cache;
                int index = Arrays.asList(DAY_OF_WEEK).indexOf(channelId.substring(0, channelId.indexOf("_")));
                if (channelId.endsWith("_enabled")) {
                    updateState(channelUID, OnOffType.from("start".equals(schedule.getCycle().get(index))));
                } else if (channelId.endsWith("_time")) {
                    final int hour = schedule.getH().get(index).intValue();
                    final int minute = schedule.getM().get(index).intValue();
                    updateState(channelUID, new DateTimeType(ZonedDateTime.now().withHour(hour).withMinute(minute)));
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for schedule values.", channelUID);
                }
            } else if (cache instanceof OpenOnly) {
                final OpenOnly openOnly = (OpenOnly) cache;
                if (CHANNEL_CONTROL_EDGE_CLEAN.equals(channelId)) {
                    final Boolean open = openOnly.getOpenOnly();
                    updateState(channelUID, open != null ? OnOffType.from(!open) : UnDefType.UNDEF);
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for edge clean values.", channelUID);
                }
            } else if (cache instanceof PowerBoost) {
                final PowerBoost boost = (PowerBoost) cache;
                if (CHANNEL_CONTROL_POWER_BOOST.equals(channelId)) {
                    if (Boolean.TRUE.equals(boost.getCarpetBoost())) {
                        // When set to true, overrides vacHigh
                        updateState(channelUID, StringType.valueOf(BOOST_AUTO));
                    } else if (Boolean.TRUE.equals(boost.getVacHigh())) {
                        updateState(channelUID, StringType.valueOf(BOOST_PERFORMANCE));
                    } else if (Boolean.FALSE.equals(boost.getVacHigh())) {
                        updateState(channelUID, StringType.valueOf(BOOST_ECO));
                    } else {
                        updateState(channelUID, UnDefType.UNDEF);
                    }
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for boost values.", channelUID);
                }
            } else {
                super.handleCommand(channelUID, command);
            }
        } else if (command instanceof OnOffType) {
            if (CHANNEL_CONTROL_EDGE_CLEAN.equals(channelId)) {
                sendSetting(new OpenOnly(command.equals(OnOffType.OFF)));
            } else if (SCHEDULE_GROUP_ID.equals(channelUID.getGroupId())) {
                int index = Arrays.asList(DAY_OF_WEEK).indexOf(channelId.substring(0, channelId.indexOf("_")));
                final CleanSchedule schedule = new CleanSchedule((CleanSchedule) getCacheEntry(channelUID));
                List<String> cycle = schedule.getCycle().stream().collect(Collectors.toList());
                cycle.set(index, command.equals(OnOffType.ON) ? "start" : "none");
                schedule.setCycle(cycle);
                Reported state = new Reported();
                state.setCleanSchedule(schedule);
                sendSetting(state);
            } else {
                super.handleCommand(channelUID, command);
            }
        } else if (command instanceof DateTimeType) {
            if (SCHEDULE_GROUP_ID.equals(channelUID.getGroupId())) {
                int index = Arrays.asList(DAY_OF_WEEK).indexOf(channelId.substring(0, channelId.indexOf("_")));
                final CleanSchedule schedule = new CleanSchedule((CleanSchedule) getCacheEntry(channelUID));
                List<BigInteger> hours = schedule.getH().stream().collect(Collectors.toList());
                hours.set(index, BigInteger.valueOf(((DateTimeType) command).getZonedDateTime().getHour()));
                schedule.setH(hours);
                List<BigInteger> minutes = schedule.getM().stream().collect(Collectors.toList());
                minutes.set(index, BigInteger.valueOf(((DateTimeType) command).getZonedDateTime().getMinute()));
                schedule.setM(minutes);
                Reported state = new Reported();
                state.setCleanSchedule(schedule);
                sendSetting(state);
            } else {
                super.handleCommand(channelUID, command);
            }
        } else if (command instanceof StringType) {
            if (CHANNEL_CONTROL_POWER_BOOST.equals(channelId)) {
                sendSetting(new PowerBoost(command.equals(BOOST_AUTO), command.equals(BOOST_PERFORMANCE)));
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

        final Boolean openOnly = reported.getOpenOnly();
        if (openOnly != null) {
            final OpenOnly open = new OpenOnly(openOnly);
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            setCacheEntry(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_EDGE_CLEAN), open);
        }

        final CleanSchedule schedule = reported.getCleanSchedule();
        if (schedule != null) {
            final ChannelGroupUID scheduleGroupUID = new ChannelGroupUID(thingUID, SCHEDULE_GROUP_ID);
            for (int i = 0; i < DAY_OF_WEEK.length; i++) {
                setCacheEntry(new ChannelUID(scheduleGroupUID, DAY_OF_WEEK[i] + "_enabled"), schedule);
                setCacheEntry(new ChannelUID(scheduleGroupUID, DAY_OF_WEEK[i] + "_time"), schedule);
            }
        }

        final Boolean vacHigh = reported.getVacHigh();
        final Boolean carpetBoost = reported.getCarpetBoost();
        if ((carpetBoost != null) || (vacHigh != null)) {
            // "vacHigh":false, "carpetBoost":false
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            final IRobotDTO cached = getCacheEntry(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_POWER_BOOST));
            PowerBoost boost = cached instanceof PowerBoost ? (PowerBoost) cached : new PowerBoost();
            if (carpetBoost != null) {
                boost.setCarpetBoost(carpetBoost);
            }
            if (vacHigh != null) {
                boost.setVacHigh(vacHigh);
            }
            setCacheEntry(new ChannelUID(controlGroupUID, CHANNEL_CONTROL_POWER_BOOST), boost);
        }

        // Properties
        updateProperty("bootloaderVer", reported.getBootloaderVer());
        updateProperty(PROPERTY_HARDWARE_VERSION, reported.getHardwareRev());
        updateProperty("mobilityVer", reported.getMobilityVer());
        updateProperty("navSwVer", reported.getNavSwVer());
        updateProperty("soundVer", reported.getSoundVer());
        updateProperty("uiSwVer", reported.getUiSwVer());
        updateProperty("umiVer", reported.getUmiVer());
        updateProperty("wifiSwVer", reported.getWifiSwVer());

        super.receive(reported);
    }
}
