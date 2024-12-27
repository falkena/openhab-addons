/*
 * Copyright (c) 2010-2026 Contributors to the openHAB project
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
package org.openhab.binding.systeminfo.internal.handler;

import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.BRIDGE_TYPE_COMPUTER_IMPL;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_BATTERY_NAME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_BATTERY_REMAINING_CAPACITY;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_BATTERY_REMAINING_TIME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_CPU_FREQUENCY;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_CPU_GROUP;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_CPU_MAXFREQUENCY;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_CPU_TEMPERATURE;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_CPU_VOLTAGE;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_CURRENT_PROCESS_LOAD;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_CURRENT_PROCESS_MEMORY;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_CURRENT_PROCESS_NAME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_CURRENT_PROCESS_PATH;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_CURRENT_PROCESS_THREADS;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_DESCRIPTION;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_DISPLAY_INFORMATION;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_GROUP_BATTERY;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_GROUP_CURRENT_PROCESS;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_GROUP_DISPLAY;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_GROUP_PROCESS;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_GROUP_TYPE_BATTERY;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_GROUP_TYPE_DISPLAY;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_HEAP_GROUP;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_MEMORY_GROUP;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NAME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_PROCESS_LOAD;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_PROCESS_MEMORY;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_PROCESS_NAME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_PROCESS_PATH;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_PROCESS_THREADS;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_SENSORS_FAN_SPEED;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_SWAP_GROUP;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_SYSTEM_GROUP;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_SYSTEM_LOAD;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_SYSTEM_LOAD_1;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_SYSTEM_LOAD_15;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_SYSTEM_LOAD_5;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_SYSTEM_THREADS;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_SYSTEM_UPTIME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.HIGH_PRIORITY_REFRESH_TIME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.MEDIUM_PRIORITY_REFRESH_TIME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.PID_PARAMETER;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.PRIORITY_PARAMETER;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.PROPERTY_CPU_LOGICAL_CORES;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.PROPERTY_CPU_PHYSICAL_CORES;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.PROPERTY_HOSTNAME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.PROPERTY_HOSTNAME_DEFAULT;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.PROPERTY_OS_FAMILY;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.PROPERTY_OS_MANUFACTURER;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.PROPERTY_OS_VERSION;
import static org.openhab.binding.systeminfo.internal.handler.SystemInfoHandlerUtilities.getChannelState;
import static org.openhab.binding.systeminfo.internal.handler.SystemInfoHandlerUtilities.round;

import java.math.BigDecimal;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.systeminfo.internal.SystemInfoThingTypeProvider;
import org.openhab.binding.systeminfo.internal.discovery.SystemInfoDeviceDiscoveryService;
import org.openhab.binding.systeminfo.internal.model.DeviceNotFoundException;
import org.openhab.binding.systeminfo.internal.model.SystemInfoInterface;
import org.openhab.core.cache.ExpiringCache;
import org.openhab.core.cache.ExpiringCacheMap;
import org.openhab.core.config.core.Configuration;
import org.openhab.core.library.types.DecimalType;
import org.openhab.core.library.types.QuantityType;
import org.openhab.core.library.types.StringType;
import org.openhab.core.library.unit.SIUnits;
import org.openhab.core.library.unit.Units;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.Channel;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingRegistry;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusDetail;
import org.openhab.core.thing.ThingTypeUID;
import org.openhab.core.thing.binding.ThingHandler;
import org.openhab.core.thing.binding.ThingHandlerService;
import org.openhab.core.thing.binding.builder.ThingBuilder;
import org.openhab.core.thing.type.ChannelGroupDefinition;
import org.openhab.core.types.Command;
import org.openhab.core.types.RefreshType;
import org.openhab.core.types.State;
import org.openhab.core.types.UnDefType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import oshi.hardware.CentralProcessor;
import oshi.hardware.GlobalMemory;
import oshi.hardware.VirtualMemory;

/**
 * The {@link SystemInfoComputerHandler} is responsible for providing real time information about the system
 * (CPU, Memory, Storage, Display and others).
 *
 * @author Svilen Valkanov - Initial contribution
 * @author Lyubomir Papzov - Separate the creation of the systeminfo object and its initialization
 * @author Wouter Born - Add null annotations
 * @author Mark Herwege - Add dynamic creation of extra channels
 * @author Mark Herwege - Processor frequency channels
 */
@NonNullByDefault
public class SystemInfoComputerHandler extends SystemInfoBridgeScheduler {
    /**
     * Wait time for the creation of Item-Channel links in seconds. This delay is needed, because the Item-Channel
     * links have to be created before the thing state is updated, otherwise item state will not be updated.
     */
    public static final int WAIT_TIME_CHANNEL_ITEM_LINK_INIT = 1;

    private final SystemInfoInterface systeminfo;
    public final SystemInfoThingTypeProvider thingTypeProvider;
    private final ThingRegistry thingRegistry;

    private @Nullable ScheduledFuture<?> highPriorityTasks;
    private @Nullable ScheduledFuture<?> mediumPriorityTasks;

    /**
     * Caches for cpu process load and process load for a given pid. Using this cache limits the process load refresh
     * interval to the minimum interval. Too frequent refreshes leads to inaccurate results. This could happen when the
     * same process is tracked as current process and as a channel with pid parameter, or when the task interval is set
     * too low.
     */
    private static final long LOAD_REFRESH_INTERVAL_MS = 2000;
    private final ExpiringCache<@Nullable BigDecimal> cpuLoad;

    private ExpiringCacheMap<Integer, @Nullable DecimalType> processLoadCache = new ExpiringCacheMap<>(
            LOAD_REFRESH_INTERVAL_MS);

    private final Logger logger = LoggerFactory.getLogger(SystemInfoComputerHandler.class);

    public SystemInfoComputerHandler(final Bridge bridge, final SystemInfoInterface systemInfo,
            final SystemInfoThingTypeProvider thingTypeProvider, final ThingRegistry thingRegistry) {
        super(bridge);
        this.systeminfo = systemInfo;
        this.thingTypeProvider = thingTypeProvider;
        this.thingRegistry = thingRegistry;
        cpuLoad = new ExpiringCache<>(LOAD_REFRESH_INTERVAL_MS, () -> {
            final CentralProcessor cpu = systeminfo.getCPUSpecification();
            return SystemInfoHandlerUtilities.getCPULoad(cpu);
        });
    }

    @Override
    public Collection<Class<? extends ThingHandlerService>> getServices() {
        return List.of(SystemInfoDeviceDiscoveryService.class);
    }

    @Override
    public void initialize() {
        logger.trace("Initializing thing {} with thing type {}", thing.getUID().getId(),
                thing.getThingTypeUID().getId());

        // After a thing type change, previous channel configs will have been stored, and will be restored here.
        logger.trace("Restoring channel configurations");
        final Map<String, Configuration> channelsConfig = thingTypeProvider.restoreChannelsConfig(thing.getUID());
        for (final String channelId : channelsConfig.keySet()) {
            final Channel channel = thing.getChannel(channelId);
            final Configuration config = channelsConfig.get(channelId);
            if ((channel != null) && (config != null)) {
                for (final String parameter : config.keySet()) {
                    if (isConfigurationKeyChanged(channel.getConfiguration(), config, parameter)) {
                        handleChannelConfigurationChange(channel, config, parameter);
                    }
                }
            }
        }

        if (instantiateSystemInfoLibrary() && isConfigurationValid() && updateProperties()) {
            // If there are new channel groups, the thing will get recreated with a new thing type and
            // this handler will be disposed. Therefore, do not do anything further here.
            if (!addDynamicChannels()) {
                for (final Channel channel : this.thing.getChannels()) {
                    final Configuration properties = channel.getConfiguration();
                    final Object priority = properties.get(PRIORITY_PARAMETER);
                    if (priority != null) {
                        changeChannelPriority(channel.getUID(), priority);
                    } else {
                        logger.debug("The channel {} has no priority set.", channel.getUID());
                        break;
                    }
                }
                scheduleUpdates();
                updateStatus(ThingStatus.ONLINE);
            }
        } else {
            updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.HANDLER_INITIALIZING_ERROR,
                    "@text/offline.cannot-initialize");
        }
    }

    @Override
    public void dispose() {
        ScheduledFuture<?> highPriorityTasks = this.highPriorityTasks;
        if (highPriorityTasks != null) {
            logger.debug("High priority tasks will not be run anymore");
            highPriorityTasks.cancel(true);
            this.highPriorityTasks = null;
        }

        ScheduledFuture<?> mediumPriorityTasks = this.mediumPriorityTasks;
        if (mediumPriorityTasks != null) {
            logger.debug("Medium priority tasks will not be run anymore");
            mediumPriorityTasks.cancel(true);
            this.mediumPriorityTasks = null;
        }
    }

    @Override
    public synchronized void thingUpdated(Thing thing) {
        logger.trace("About to update thing");
        boolean isChannelConfigChanged = false;

        List<Channel> channels = thing.getChannels();

        for (Channel channel : channels) {
            ChannelUID channelUID = channel.getUID();
            Configuration newChannelConfig = channel.getConfiguration();
            Channel oldChannel = this.thing.getChannel(channelUID.getId());

            if (oldChannel == null) {
                logger.warn("Channel with UID {} cannot be updated, as it cannot be found!", channelUID);
                continue;
            }
            Configuration currentChannelConfig = oldChannel.getConfiguration();

            if (isConfigurationKeyChanged(currentChannelConfig, newChannelConfig, PRIORITY_PARAMETER)) {
                isChannelConfigChanged = true;

                handleChannelConfigurationChange(oldChannel, newChannelConfig, PRIORITY_PARAMETER);
                changeChannelPriority(channelUID, newChannelConfig.get(PRIORITY_PARAMETER));
            }

            if (isConfigurationKeyChanged(currentChannelConfig, newChannelConfig, PID_PARAMETER)) {
                isChannelConfigChanged = true;
                handleChannelConfigurationChange(oldChannel, newChannelConfig, PID_PARAMETER);
            }
        }

        if (!(isInitialized() && isChannelConfigChanged)) {
            super.thingUpdated(thing);
        }
    }

    @Override
    public void handleRemoval() {
        thingTypeProvider.removeThingType(thing.getThingTypeUID());
        super.handleRemoval();
    }

    @Override
    public void handleCommand(ChannelUID channelUID, Command command) {
        if (thing.getStatus().equals(ThingStatus.ONLINE)) {
            if (command instanceof RefreshType) {
                logger.debug("Refresh command received for channel {} !", channelUID);
                if (CHANNEL_CPU_GROUP.equals(channelUID.getGroupId())) {
                    final State state = switch (channelUID.getIdWithoutGroup()) {
                        case CHANNEL_CPU_MAXFREQUENCY -> {
                            final long value = systeminfo.getCPUSpecification().getMaxFreq();
                            yield value > 0 ? new QuantityType<>(value, Units.HERTZ) : UnDefType.UNDEF;
                        }
                        case CHANNEL_CPU_TEMPERATURE -> {
                            final BigDecimal value = round(systeminfo.getSensors().getCpuTemperature());
                            yield value.signum() == 1 ? new QuantityType<>(value, SIUnits.CELSIUS) : UnDefType.UNDEF;
                        }
                        case CHANNEL_CPU_VOLTAGE -> {
                            final BigDecimal voltage = round(systeminfo.getSensors().getCpuVoltage());
                            yield voltage.signum() == 1 ? new QuantityType<>(voltage, Units.VOLT) : UnDefType.UNDEF;
                        }
                        case CHANNEL_DESCRIPTION -> {
                            final CentralProcessor.ProcessorIdentifier identifier = systeminfo.getCPUIdentifier();
                            final String format = "Model: %s %s,family: %s, vendor: %s, sn: %s, identifier: %s ";
                            yield new StringType(String.format(format, identifier.getModel(),
                                    identifier.isCpu64bit() ? "64 bit" : "32 bit", identifier.getFamily(),
                                    identifier.getVendor(), systeminfo.getSystem().getSerialNumber(),
                                    identifier.getIdentifier()));
                        }
                        case CHANNEL_NAME -> new StringType(systeminfo.getCPUIdentifier().getName());
                        default -> UnDefType.UNDEF;
                    };
                    if (channelUID.getId().contains(CHANNEL_CPU_FREQUENCY)) {
                        int deviceIndex = getDeviceIndex(channelUID);
                        final long[] frequencies = systeminfo.getCPUSpecification().getCurrentFreq();
                        if ((deviceIndex < frequencies.length) && (frequencies[deviceIndex] > 0)) {
                            updateState(channelUID, new QuantityType<>(frequencies[deviceIndex], Units.HERTZ));
                        } else {
                            updateState(channelUID, UnDefType.UNDEF);
                        }
                    } else {
                        updateState(channelUID, state);
                    }
                } else if (CHANNEL_MEMORY_GROUP.equals(channelUID.getGroupId())) {
                    final GlobalMemory memory = systeminfo.getMemorySpecifications();
                    final long available = memory.getAvailable();
                    final long total = memory.getTotal();
                    updateState(channelUID, getChannelState(channelUID, available, total - available, total));
                } else if (CHANNEL_HEAP_GROUP.equals(channelUID.getGroupId())) {
                    final Runtime runtime = Runtime.getRuntime();
                    final long free = runtime.freeMemory();
                    final long total = runtime.totalMemory();
                    updateState(channelUID, getChannelState(channelUID, free, total - free, total));
                } else if (CHANNEL_SWAP_GROUP.equals(channelUID.getGroupId())) {
                    final VirtualMemory swap = systeminfo.getSwapSpecifications();
                    final long used = swap.getSwapUsed();
                    final long total = swap.getSwapTotal();
                    updateState(channelUID, getChannelState(channelUID, total - used, used, total));
                } else if (CHANNEL_SYSTEM_GROUP.equals(channelUID.getGroupId())) {
                    final State state = switch (channelUID.getIdWithoutGroup()) {
                        case CHANNEL_SYSTEM_LOAD -> {
                            final BigDecimal value = cpuLoad.getValue();
                            yield value != null ? new QuantityType<>(value, Units.PERCENT) : UnDefType.UNDEF;
                        }
                        case CHANNEL_SYSTEM_LOAD_1 -> {
                            final double[] value = systeminfo.getCPUSpecification().getSystemLoadAverage(3);
                            yield (value.length == 3) && (value[0] > 0) ? new DecimalType(round(value[0]))
                                    : UnDefType.UNDEF;
                        }
                        case CHANNEL_SYSTEM_LOAD_5 -> {
                            final double[] value = systeminfo.getCPUSpecification().getSystemLoadAverage(3);
                            yield (value.length == 3) && (value[1] > 0) ? new DecimalType(round(value[1]))
                                    : UnDefType.UNDEF;
                        }
                        case CHANNEL_SYSTEM_LOAD_15 -> {
                            final double[] value = systeminfo.getCPUSpecification().getSystemLoadAverage(3);
                            yield (value.length == 3) && (value[2] > 0) ? new DecimalType(round(value[2]))
                                    : UnDefType.UNDEF;
                        }
                        case CHANNEL_SYSTEM_THREADS -> {
                            final int value = systeminfo.getOperatingSystem().getThreadCount();
                            yield value > 0 ? new DecimalType(value) : UnDefType.UNDEF;
                        }
                        case CHANNEL_SYSTEM_UPTIME -> {
                            final long value = systeminfo.getOperatingSystem().getSystemUptime();
                            yield value > 0 ? new QuantityType<>(value, Units.SECOND) : UnDefType.UNDEF;
                        }
                        default -> UnDefType.UNDEF;
                    };
                    updateState(channelUID, state);
                } else {
                    updateState(channelUID, getInfoForChannel(channelUID));
                }
            } else {
                logger.debug("Unsupported command {} ! Supported commands: REFRESH", command);
            }
        } else {
            logger.debug("Cannot handle command. Thing is not ONLINE.");
        }
    }

    public SystemInfoInterface getSystemInfo() {
        return systeminfo;
    }

    private boolean instantiateSystemInfoLibrary() {
        try {
            systeminfo.initializeSystemInfo();
            logger.debug("SystemInfo implementation is instantiated!");
            return true;
        } catch (Exception e) {
            logger.warn("Cannot instantiate SystemInfo object!", e);
            return false;
        }
    }

    private boolean isConfigurationValid() {
        logger.debug("Start reading Thing configuration.");
        try {
            final Configuration configuration = thing.getConfiguration();
            if (configuration.get(HIGH_PRIORITY_REFRESH_TIME) instanceof BigDecimal highPriorityInterval) {
                setHighPriorityRefreshInterval(highPriorityInterval);
            } else {
                throw new IllegalArgumentException("Refresh time object type is wrong");
            }
            if (configuration.get(MEDIUM_PRIORITY_REFRESH_TIME) instanceof BigDecimal mediumPriorityInterval) {
                setMediumPriorityRefreshInterval(mediumPriorityInterval);
            } else {
                throw new IllegalArgumentException("Refresh time object type is wrong");
            }

            if ((getHighPriorityRefreshInterval() <= 0) || (getMediumPriorityRefreshInterval() <= 0)) {
                throw new IllegalArgumentException("Refresh time must be positive number");
            }
            logger.debug("Refresh time for high priority channels set to {} s", highPriorityInterval);
            logger.debug("Refresh time for medium priority channels set to {} s", mediumPriorityInterval);
            return true;
        } catch (IllegalArgumentException exception) {
            logger.warn("{}. Please change the thing configuration", exception.getLocalizedMessage());
        } catch (ClassCastException exception) {
            logger.debug("Channel configuration cannot be read");
        }
        return false;
    }

    private boolean updateProperties() {
        Map<String, String> properties = editProperties();
        try {
            final CentralProcessor cpu = systeminfo.getCPUSpecification();
            properties.put(PROPERTY_CPU_LOGICAL_CORES, Integer.toString(cpu.getLogicalProcessorCount()));
            properties.put(PROPERTY_CPU_PHYSICAL_CORES, Integer.toString(cpu.getPhysicalProcessorCount()));
            properties.put(PROPERTY_OS_FAMILY, systeminfo.getOsFamily().toString());
            properties.put(PROPERTY_OS_MANUFACTURER, systeminfo.getOsManufacturer().toString());
            properties.put(PROPERTY_OS_VERSION, systeminfo.getOsVersion().toString());
            try {
                properties.put(PROPERTY_HOSTNAME, InetAddress.getLocalHost().getHostName());
            } catch (UnknownHostException exception) {
                properties.put(PROPERTY_HOSTNAME, PROPERTY_HOSTNAME_DEFAULT);
            }

            updateProperties(properties);
            logger.debug("Properties updated!");
            return true;
        } catch (Exception exception) {
            logger.debug("Cannot get system properties! Please try to restart the binding.", exception);
            return false;
        }
    }

    /**
     * Retrieve info on available storages, drives, displays, batteries, network interfaces and fans in the system. If
     * there is more than 1, create additional channel groups and channels representing each of the entities with an
     * index added to the channel groups and channels. The base channel groups and channels will remain without index
     * and are equal to the channel groups and channels with index 0. If there is only one entity in a group, do not add
     * a channels group and channels with index 0.
     * <p>
     * If channel groups are added, the thing type will change to
     * {@link org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants#BRIDGE_TYPE_COMPUTER_IMPL
     * computer-impl}. A new handler will be created and initialization restarted.
     * Therefore further initialization of the current handler can be aborted if the method returns true.
     *
     * @return true if channel groups where added
     */
    private boolean addDynamicChannels() {
        List<ChannelGroupDefinition> newChannelGroups = new ArrayList<>();
        addChannelGroups(CHANNEL_GROUP_DISPLAY, CHANNEL_GROUP_TYPE_DISPLAY, systeminfo.getDisplayCount(),
                newChannelGroups);
        addChannelGroups(CHANNEL_GROUP_BATTERY, CHANNEL_GROUP_TYPE_BATTERY, systeminfo.getPowerSourceCount(),
                newChannelGroups);
        if (!newChannelGroups.isEmpty()) {
            logger.debug("Creating additional channel groups");
            newChannelGroups.addAll(0, thingTypeProvider.getChannelGroupDefinitions(thing.getThingTypeUID()));
            thingTypeProvider.updateThingType(BRIDGE_TYPE_COMPUTER_IMPL, newChannelGroups);
            logger.trace("Channel groups were added, changing the thing type");
            changeThingType(BRIDGE_TYPE_COMPUTER_IMPL, thing.getConfiguration());
            return true;
        }

        List<Channel> newChannels = new ArrayList<>();
        addChannels(CHANNEL_SENSORS_FAN_SPEED, systeminfo.getFanCount(), newChannels);

        final CentralProcessor cpu = systeminfo.getCPUSpecification();
        addChannels(CHANNEL_CPU_FREQUENCY, cpu.getLogicalProcessorCount(), newChannels);
        if (!newChannels.isEmpty()) {
            logger.debug("Creating additional channels");
            newChannels.addAll(0, thing.getChannels());
            ThingBuilder thingBuilder = editThing();
            thingBuilder.withChannels(newChannels);
            updateThing(thingBuilder.build());
        }

        return false;
    }

    private void addChannelGroups(String channelGroupID, String channelGroupTypeID, int count,
            List<ChannelGroupDefinition> groups) {
        if (count <= 1) {
            return;
        }

        List<String> channelGroups = thingTypeProvider.getChannelGroupDefinitions(thing.getThingTypeUID()).stream()
                .map(ChannelGroupDefinition::getId).toList();

        for (int i = 0; i < count; i++) {
            String index = String.valueOf(i);
            ChannelGroupDefinition channelGroupDef = thingTypeProvider
                    .createChannelGroupDefinitionWithIndex(channelGroupID, channelGroupTypeID, i);
            if (!(channelGroupDef == null || channelGroups.contains(channelGroupID + index))) {
                logger.trace("Adding channel group {}", channelGroupID + index);
                groups.add(channelGroupDef);
            }
        }
    }

    private void addChannels(String channelID, int count, List<Channel> channels) {
        if (count <= 1) {
            return;
        }

        for (int i = 0; i < count; i++) {
            Channel channel = thingTypeProvider.createChannelWithIndex(thing, channelID, i);
            if (channel != null && thing.getChannel(channel.getUID()) == null) {
                logger.trace("Creating channel {}", channel.getUID().getId());
                channels.add(channel);
            }
        }
    }

    private void scheduleUpdates() {
        final int highPriorityInterval = getHighPriorityRefreshInterval();
        highPriorityTasks = scheduler.scheduleWithFixedDelay(() -> {
            logger.debug("Schedule high priority tasks at fixed rate {} s", highPriorityInterval);
            publishData(getHighPriorityChannels());
        }, WAIT_TIME_CHANNEL_ITEM_LINK_INIT, highPriorityInterval, TimeUnit.SECONDS);

        final int mediumPriorityInterval = getMediumPriorityRefreshInterval();
        mediumPriorityTasks = scheduler.scheduleWithFixedDelay(() -> {
            logger.debug("Schedule medium priority tasks at fixed rate {} s", mediumPriorityInterval);
            publishData(getMediumPriorityChannels());
        }, WAIT_TIME_CHANNEL_ITEM_LINK_INIT, mediumPriorityInterval, TimeUnit.SECONDS);

        logger.debug("Schedule one time update for low priority tasks");
        scheduler.schedule(() -> {
            publishData(getLowPriorityChannels());
        }, WAIT_TIME_CHANNEL_ITEM_LINK_INIT, TimeUnit.SECONDS);
    }

    private void publishData(final Set<ChannelUID> channels) {
        if (ThingStatus.ONLINE.equals(thing.getStatus())) {
            for (final ChannelUID channelUID : channels) {
                final Thing thing = thingRegistry.get(channelUID.getThingUID());
                if (thing != null) {
                    final ThingHandler handler = thing.getHandler();
                    if (handler != null) {
                        handler.handleCommand(channelUID, RefreshType.REFRESH);
                    } else {
                        logger.warn("Tried to update channel {} on not configured thing {}", channelUID,
                                thing.getUID());
                    }
                } else {
                    logger.warn("Tried to update channel {} on not available thing", channelUID);
                }
            }
        }
    }

    /**
     * This method gets the information for specific channel through the {@link SystemInfoInterface}. It uses the
     * channel ID to call the correct method from the {@link SystemInfoInterface} with deviceIndex parameter (in case of
     * multiple devices, for reference see {@link SystemInfoComputerHandler#getDeviceIndex(ChannelUID)}})
     *
     * @param channelUID the UID of the channel
     * @return State object or null, if there is no information for the device with this index
     */
    private State getInfoForChannel(ChannelUID channelUID) {
        State state = null;

        String channelID = channelUID.getId();
        int deviceIndex = getDeviceIndex(channelUID);

        logger.trace("Getting state for channel {} with device index {}", channelID, deviceIndex);

        // The channelGroup or channel may contain deviceIndex. It must be deleted from the channelID, because otherwise
        // the switch will not find the correct method below.
        // All digits are deleted from the ID, except for system load channels.
        channelID = switch (channelID) {
            case CHANNEL_SYSTEM_LOAD_1, CHANNEL_SYSTEM_LOAD_5, CHANNEL_SYSTEM_LOAD_15 -> channelID;
            default -> channelID.replaceAll("\\d+", "");
        };

        try {
            switch (channelID) {
                case CHANNEL_DISPLAY_INFORMATION:
                    state = systeminfo.getDisplayInformation(deviceIndex);
                    break;
                case CHANNEL_BATTERY_NAME:
                    state = systeminfo.getBatteryName(deviceIndex);
                    break;
                case CHANNEL_BATTERY_REMAINING_CAPACITY:
                    state = new QuantityType<>(systeminfo.getBatteryRemainingCapacity(deviceIndex), Units.PERCENT);
                    break;
                case CHANNEL_BATTERY_REMAINING_TIME:
                    state = systeminfo.getBatteryRemainingTime(deviceIndex);
                    break;
                case CHANNEL_PROCESS_LOAD:
                case CHANNEL_CURRENT_PROCESS_LOAD:
                    DecimalType processLoad = processLoadCache.putIfAbsentAndGet(deviceIndex,
                            () -> getProcessCpuUsage(deviceIndex));
                    state = (processLoad != null) ? new QuantityType<>(processLoad, Units.PERCENT) : null;
                    break;
                case CHANNEL_PROCESS_MEMORY:
                case CHANNEL_CURRENT_PROCESS_MEMORY:
                    state = systeminfo.getProcessMemoryUsage(deviceIndex);
                    break;
                case CHANNEL_PROCESS_NAME:
                case CHANNEL_CURRENT_PROCESS_NAME:
                    state = systeminfo.getProcessName(deviceIndex);
                    break;
                case CHANNEL_PROCESS_PATH:
                case CHANNEL_CURRENT_PROCESS_PATH:
                    state = systeminfo.getProcessPath(deviceIndex);
                    break;
                case CHANNEL_PROCESS_THREADS:
                case CHANNEL_CURRENT_PROCESS_THREADS:
                    state = systeminfo.getProcessThreads(deviceIndex);
                    break;
                case CHANNEL_SENSORS_FAN_SPEED:
                    state = systeminfo.getSensorsFanSpeed(deviceIndex);
                    break;
                default:
                    logger.debug("Channel with unknown ID: {} !", channelID);
            }
        } catch (DeviceNotFoundException exception) {
            if (isLinked(channelUID)) {
                logger.warn("No information for channel {} with device index: {}", channelID, deviceIndex);
            }
        } catch (Exception exception) {
            logger.debug("Unexpected error occurred while getting system information!", exception);
            updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.COMMUNICATION_ERROR, "@text/offline.unexpected-error");
        }
        return state != null ? state : UnDefType.UNDEF;
    }

    private @Nullable DecimalType getProcessCpuUsage(int pid) {
        try {
            return systeminfo.getProcessCpuUsage(pid);
        } catch (DeviceNotFoundException e) {
            logger.warn("Process with pid {} does not exist", pid);
            return null;
        }
    }

    /**
     * The device index is an optional part of the channelID - the last characters of the groupID. It is used to
     * identify unique device, when more than one devices are available (e.g. local disks with names C:\, D:\, E"\ - the
     * first will have deviceIndex=0, the second deviceIndex=1 ant etc).
     * When no device index is specified, default value of 0 (first device in the list) is returned.
     *
     * @param channelUID the ID of the channel
     * @return natural number (number >=0)
     */
    private int getDeviceIndex(ChannelUID channelUID) {
        String channelID = channelUID.getId();
        String channelGroupID = channelUID.getGroupId();
        if (channelGroupID == null) {
            return 0;
        }

        if (channelGroupID.contains(CHANNEL_GROUP_PROCESS)) {
            // Only in this case the deviceIndex is part of the channel configuration - PID (Process Identifier)
            int pid = getPID(channelUID);
            logger.debug("Channel with UID {} tracks process with PID: {}", channelUID, pid);
            return pid;
        }

        if (channelGroupID.contains(CHANNEL_GROUP_CURRENT_PROCESS)) {
            return systeminfo.getCurrentProcessID();
        }

        // First try to get device index in group id, delete all non-digits from id
        if (Character.isDigit(channelGroupID.charAt(channelGroupID.length() - 1))) {
            String deviceIndexPart = channelGroupID.replaceAll("\\D+", "");
            return Integer.parseInt(deviceIndexPart);
        }

        // If not found, try to find it in channel id, delete all non-digits from id
        if (Character.isDigit(channelID.charAt(channelID.length() - 1))) {
            String deviceIndexPart = channelID.replaceAll("\\D+", "");
            return Integer.parseInt(deviceIndexPart);
        }

        return 0;
    }

    /**
     * This method gets the process identifier (PID) for specific process
     *
     * @param channelUID channel unique identifier
     * @return natural number
     */
    private int getPID(ChannelUID channelUID) {
        int pid = 0;
        try {
            Channel channel = this.thing.getChannel(channelUID.getId());
            if (channel != null) {
                Configuration channelProperties = channel.getConfiguration();
                BigDecimal pidValue = (BigDecimal) channelProperties.get(PID_PARAMETER);
                if (pidValue == null || pidValue.intValue() < 0) {
                    throw new IllegalArgumentException("Invalid value for Process Identifier.");
                } else {
                    pid = pidValue.intValue();
                }
            } else {
                logger.debug("Channel does not exist! Fall back to default value.");
            }
        } catch (ClassCastException e) {
            logger.debug("Channel configuration cannot be read! Fall back to default value.", e);
        } catch (IllegalArgumentException e) {
            logger.debug("PID (Process Identifier) must be positive number. Fall back to default value. ", e);
        }
        return pid;
    }

    private boolean isConfigurationKeyChanged(Configuration currentConfig, Configuration newConfig, String key) {
        Object currentValue = currentConfig.get(key);
        Object newValue = newConfig.get(key);

        if (currentValue == null) {
            return (newValue != null);
        }

        return !currentValue.equals(newValue);
    }

    private void handleChannelConfigurationChange(final Channel channel, final Configuration newConfig,
            final String parameter) {
        final Configuration configuration = channel.getConfiguration();
        final Object oldValue = configuration.get(parameter);

        configuration.put(parameter, newConfig.get(parameter));

        final Object newValue = newConfig.get(parameter);
        logger.debug("Channel with UID {} has changed its {} from {} to {}", channel.getUID(), parameter, oldValue,
                newValue);
        handleCommand(channel.getUID(), RefreshType.REFRESH);
    }

    // Don't remove this override. If absent channels will not be populated properly
    @Override
    protected void changeThingType(ThingTypeUID thingTypeUID, Configuration configuration) {
        logger.trace("Storing channel configurations");
        thingTypeProvider.storeChannelsConfig(thing);
        super.changeThingType(thingTypeUID, configuration);
    }
}
