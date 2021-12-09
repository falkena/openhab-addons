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
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_CONTROL_LANGUAGE;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_CONTROL_POWER_BOOST;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_NETWORK_ADDRESS;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_NETWORK_DNS1;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_NETWORK_DNS2;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_NETWORK_GATEWAY;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_NETWORK_MASK;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CONTROL_GROUP_ID;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.DAY_OF_WEEK;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.SCHEDULE_GROUP_ID;
import static org.openhab.core.thing.Thing.PROPERTY_HARDWARE_VERSION;

import java.math.BigInteger;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.irobot.internal.IRobotChannelContentProvider;
import org.openhab.binding.irobot.internal.dto.CleanSchedule;
import org.openhab.binding.irobot.internal.dto.IRobotDTO;
import org.openhab.binding.irobot.internal.dto.Languages1;
import org.openhab.binding.irobot.internal.dto.NetInfo;
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
 * @author hkuhn42 - Initial contribution
 * @author Pavel Fedin - Rewrite for 900 series
 * @author Florian Binder - added cleanRegions command and lastCommand channel
 * @author Alexander Falkenstern - Introduce handler for 9-series vacuum robots
 */
@NonNullByDefault
public class Roomba9ModelsHandler extends RoombaCommonHandler {
    private final Logger logger = LoggerFactory.getLogger(Roomba9ModelsHandler.class);

    private IRobotChannelContentProvider channelContentProvider;

    public Roomba9ModelsHandler(Thing thing, IRobotChannelContentProvider channelContentProvider) {
        super(thing, channelContentProvider);
        this.channelContentProvider = channelContentProvider;
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
            } else if (cache instanceof Languages1) {
                final Languages1 languages1 = (Languages1) cache;
                if (CHANNEL_CONTROL_LANGUAGE.equals(channelId)) {
                    final BigInteger language = languages1.getLanguage();
                    if (channelContentProvider.isChannelPopulated(channelUID)) {
                        updateState(channelUID, language != null ? language.toString() : null);
                    } else {
                        updateState(channelUID, UnDefType.UNDEF);
                    }
                } else if (logger.isTraceEnabled()) {
                    logger.trace("Received unknown channel {} for edge clean values.", channelUID);
                }
            } else if (cache instanceof NetInfo) {
                final NetInfo netInfo = (NetInfo) cache;
                if (CHANNEL_NETWORK_ADDRESS.equals(channelId)) {
                    updateState(channelUID, convertNumber2IP(netInfo.getAddr()));
                } else if (CHANNEL_NETWORK_DNS1.equals(channelId)) {
                    updateState(channelUID, convertNumber2IP(netInfo.getDns1()));
                } else if (CHANNEL_NETWORK_DNS2.equals(channelId)) {
                    updateState(channelUID, convertNumber2IP(netInfo.getDns2()));
                } else if (CHANNEL_NETWORK_GATEWAY.equals(channelId)) {
                    updateState(channelUID, convertNumber2IP(netInfo.getGw()));
                } else if (CHANNEL_NETWORK_MASK.equals(channelId)) {
                    updateState(channelUID, convertNumber2IP(netInfo.getMask()));
                } else {
                    super.handleCommand(channelUID, command);
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
                        updateState(channelUID, BOOST_AUTO);
                    } else if (Boolean.TRUE.equals(boost.getVacHigh())) {
                        updateState(channelUID, BOOST_PERFORMANCE);
                    } else if (Boolean.FALSE.equals(boost.getVacHigh())) {
                        updateState(channelUID, BOOST_ECO);
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
            } else if (CHANNEL_CONTROL_LANGUAGE.equals(channelId)) {
                sendSetting(new Languages1(new BigInteger(command.toString()), null));
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

        final BigInteger language = reported.getLanguage();
        final List<Map<String, Number>> languages = reported.getLangs();
        if ((language != null) || ((languages != null) && !languages.isEmpty())) {
            final ChannelGroupUID controlGroupUID = new ChannelGroupUID(thingUID, CONTROL_GROUP_ID);
            final ChannelUID languageChannelUID = new ChannelUID(controlGroupUID, CHANNEL_CONTROL_LANGUAGE);

            final IRobotDTO cached = getCacheEntry(languageChannelUID);
            Languages1 languages1 = cached instanceof Languages1 ? (Languages1) cached : new Languages1();

            if (language != null) {
                languages1.setLanguage(language);
            }

            if (languages != null) {
                if (!channelContentProvider.isChannelPopulated(languageChannelUID)) {
                    final Map<String, String> buffer = new HashMap<>();
                    for (final Map<String, Number> lang : languages) {
                        final String[] keys = lang.keySet().toArray(new String[lang.size()]);
                        buffer.put(String.valueOf(lang.get(keys[0])), keys[0]);
                    }
                    channelContentProvider.setLanguages(languageChannelUID, buffer);
                }
                languages1.setLangs(languages);
            }
            setCacheEntry(languageChannelUID, languages1);
        }

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

    private @Nullable String convertNumber2IP(@Nullable String number) {
        String result = null;
        if ((number != null) && !number.equalsIgnoreCase("0")) {
            result = new String();
            long value = Long.parseLong(number, 10);
            while (value > 0) {
                result = value % 256 + "." + result;
                value = value / 256;
            }
            result = result.substring(0, result.length() - 1);
        }
        return result;
    }
}
