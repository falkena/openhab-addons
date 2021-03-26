/**
 * Copyright (c) 2021- Alexander Falkenstern
 *
 * License: https://www.gnu.org/licenses/gpl-3.0.txt
 */
package org.openhab.binding.irobot.internal.handler;

import static org.openhab.binding.irobot.internal.IRobotBindingConstants.*;

import java.math.BigDecimal;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.Iterator;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.binding.irobot.internal.IRobotChannelContentProvider;
import org.openhab.binding.irobot.internal.utils.JSONUtils;
import org.openhab.binding.irobot.internal.utils.Requests;
import org.openhab.core.library.types.DateTimeType;
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
import org.openhab.core.types.State;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.*;

/**
 * The {@link RoombaIModelsHandler} is responsible for handling commands, which are
 * sent to one of the channels.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class RoombaIModelsHandler extends RoombaCommonHandler {
    private final Logger logger = LoggerFactory.getLogger(RoombaIModelsHandler.class);

    public RoombaIModelsHandler(Thing thing, IRobotChannelContentProvider channelContentProvider) {
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
        if (command instanceof OnOffType) {
            final JsonObject state = new JsonObject();
            if (SCHEDULE_GROUP_ID.equals(channelUID.getGroupId())) {
                /*
                 * final ChannelGroupUID groupUID = new ChannelGroupUID(channelUID.getThingUID(), SCHEDULE_GROUP_ID);
                 * final State hour = getCacheEntry(new ChannelUID(groupUID, CHANNEL_SCHEDULE_HOUR));
                 * final State minute = getCacheEntry(new ChannelUID(groupUID, CHANNEL_SCHEDULE_MINUTE));
                 * 
                 * final JsonArray day = new JsonArray();
                 * for (int i = 0; i < CHANNEL_SCHEDULE_DAYS.length; i++) {
                 * // final ChannelUID uid = new ChannelUID(groupUID, CHANNEL_SCHEDULE_DAYS[i]);
                 * // day.add(OnOffType.ON.equals(getCacheEntry(uid)) ? "start" : "none");
                 * // if (uid.equals(channelUID)) {
                 * // day.set(i, new JsonPrimitive(OnOffType.ON.equals(command) ? "start" : "none"));
                 * // }
                 * // hours.add(hour != null ? hour.as(DecimalType.class) : 12);
                 * // minutes.add(minute != null ? minute.as(DecimalType.class) : 0);
                 * }
                 * 
                 * final JsonArray schedule = new JsonArray();
                 * // schedule.add("enabled", false);
                 * // schedule.add("type", 0);
                 * // schedule.add("start", {"day": [3, 5], "hour": 10, "min": 0});
                 * // schedule.add("cmd", {"command": "start"});
                 * state.add("cleanSchedule2", schedule);
                 * sendRequest(new Requests.DeltaRequest(state));
                 */
            } else if (CHANNEL_CONTROL_MAP_LEARN.equals(channelUID.getIdWithoutGroup())) {
                state.addProperty("pmapLearningAllowed", command.equals(OnOffType.ON));
                connection.send(new Requests.DeltaRequest(state));
            } else {
                super.handleCommand(channelUID, command);
            }
        } else if (command instanceof DateTimeType) {
            /*
             * final JsonObject state = new JsonObject();
             * if (SCHEDULE_GROUP_ID.equals(channelUID.getGroupId())) {
             * 
             * final JsonArray schedule = new JsonArray();
             * // schedule.add("enabled", false);
             * // schedule.add("type", 0);
             * // schedule.add("start", {"day": [3, 5], "hour": 10, "min": 0});
             * // schedule.add("cmd", {"command": "start"});
             * state.add("cleanSchedule2", schedule);
             * sendRequest(new Requests.DeltaRequest(state));
             * } else {
             * super.handleCommand(channelUID, command);
             * }
             */
        } else if (command instanceof StringType) {
            final String channelId = channelUID.getIdWithoutGroup();
            if (CHANNEL_CONTROL_COMMAND.equals(channelId)) {
                Boolean isPaused = Boolean.FALSE;
                final ChannelGroupUID groupUID = new ChannelGroupUID(channelUID.getThingUID(), INTERNAL_GROUP_ID);
                final State lastCommand = getCacheEntry(new ChannelUID(groupUID, CHANNEL_INTERNAL_LAST_COMMAND));
                if (lastCommand != null) {
                    final JsonElement tree = JsonParser.parseString(lastCommand.toString());
                    isPaused = JSONUtils.getAsBoolean("command", tree);
                }

                String request = command.toString();
                if (COMMAND_CLEAN.equals(request)) {
                    request = (isPaused != null) && isPaused ? COMMAND_RESUME : COMMAND_START;
                }
                connection.send(new Requests.CommandRequest(new JsonPrimitive(request)));
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

        // @formatter:off
        // "cleanSchedule2": [{
        //   "enabled": false, "type": 0, "start": {"day": [3, 5], "hour": 10, "min": 0},
        //   "cmd": {"command": "start"}
        // }]
        // @formatter:on
        final JsonElement schedule = JSONUtils.find("cleanSchedule2", tree);
        if (schedule != null) {
            ZonedDateTime[] time = new ZonedDateTime[DAY_OF_WEEK.length];
            Boolean[] enabled = new Boolean[DAY_OF_WEEK.length];
            Arrays.fill(enabled, Boolean.FALSE);

            final JsonArray schedulers = schedule.getAsJsonArray();
            for (int i = 0; i < schedulers.size(); i++) {
                final Boolean enable = JSONUtils.getAsBoolean("enabled", schedulers.get(i));
                final JsonElement start = JSONUtils.find("start", schedulers.get(i));
                final JsonElement days = (start != null) ? JSONUtils.find("day", start) : null;
                if ((enable != null) && (start != null) && (days != null) && days.isJsonArray()) {
                    final BigDecimal hour = JSONUtils.getAsDecimal("hour", start);
                    final BigDecimal minute = JSONUtils.getAsDecimal("min", start);
                    Iterator<JsonElement> dayOfWeek = days.getAsJsonArray().iterator();
                    while (dayOfWeek.hasNext()) {
                        int day = dayOfWeek.next().getAsJsonPrimitive().getAsInt();
                        enabled[day] = enabled[day] || enable;
                        if (time[day] == null) {
                            time[day] = ZonedDateTime.now();
                            time[day] = time[day].withHour(hour != null ? hour.intValue() : DEFAULT_HOUR);
                            time[day] = time[day].withMinute(minute != null ? minute.intValue() : DEFAULT_MINUTE);
                        } else if (enable) {
                            time[day] = time[day].withHour(hour != null ? hour.intValue() : DEFAULT_HOUR);
                            time[day] = time[day].withMinute(minute != null ? minute.intValue() : DEFAULT_MINUTE);
                        }
                    }
                }
            }

            final ChannelGroupUID groupUID = new ChannelGroupUID(thingUID, SCHEDULE_GROUP_ID);
            for (int i = 0; i < DAY_OF_WEEK.length; i++) {
                updateState(new ChannelUID(groupUID, DAY_OF_WEEK[i] + "_enabled"), enabled[i]);
                if (time[i] != null) {
                    updateState(new ChannelUID(groupUID, DAY_OF_WEEK[i] + "_time"), time[i]);
                }
            }
        }

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

        final JsonElement lastCommand = JSONUtils.find("lastCommand", tree);
        if (lastCommand != null) {
            // @formatter:off
            // "lastCommand": {
            //   "command": "start/ui",
            //   "time": 1609233310,
            //   "initiator": "rmtApp/schedule/manual/admin",
            //   "event": null,
            //   "robot_id": "<blid>",
            //   "select_all": false,
            //   "ordered": 1,
            //   "pmap_id": "ZECXECzyRrCcUHeqlPirJQ",
            //   "regions": [{"region_id": "6", "type": "rid"}],
            //   "user_pmapv_id": "201130T220433"
            // }
            // @formatter:on
            final String commandJSON = lastCommand.toString();
            updateState(new ChannelUID(thingUID, INTERNAL_GROUP_ID, CHANNEL_INTERNAL_LAST_COMMAND), commandJSON);

            String command = JSONUtils.getAsString("command", lastCommand);
            if (COMMAND_RESUME.equals(command) || COMMAND_START.equals(command)) {
                command = COMMAND_CLEAN;
            }
            if (!COMMAND_UI.equals(command) && !COMMAND_FIND.equals(command)) { // Ignore for the moment UI command
                updateState(new ChannelUID(thingUID, CONTROL_GROUP_ID, CHANNEL_CONTROL_COMMAND), command);
            }
        }

        super.receive(topic, tree);
    }
}
