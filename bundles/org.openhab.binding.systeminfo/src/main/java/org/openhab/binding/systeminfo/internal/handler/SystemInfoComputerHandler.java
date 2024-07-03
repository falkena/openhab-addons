/*
 * Copyright (c) 2010-2025 Contributors to the openHAB project
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

import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.*;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
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
import org.openhab.core.library.types.PercentType;
import org.openhab.core.library.types.QuantityType;
import org.openhab.core.library.unit.Units;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.Channel;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusDetail;
import org.openhab.core.thing.ThingTypeUID;
import org.openhab.core.thing.binding.BaseBridgeHandler;
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
public class SystemInfoComputerHandler extends BaseBridgeHandler {
    /**
     * Refresh interval for {@link #highPriorityChannels} in seconds.
     */
    private @NonNullByDefault({}) BigDecimal refreshIntervalHighPriority;

    /**
     * Refresh interval for {@link #mediumPriorityChannels} in seconds.
     */
    private @NonNullByDefault({}) BigDecimal refreshIntervalMediumPriority;

    /**
     * Channels with priority configuration parameter set to High. They usually need frequent update of the state like
     * CPU load, or information about the free and used memory.
     * They are updated periodically at {@link #refreshIntervalHighPriority}.
     */
    private final Set<ChannelUID> highPriorityChannels = new HashSet<>();

    /**
     * Channels with priority configuration parameter set to Medium. These channels usually need update of the
     * state not so oft like battery capacity, storage used and etc.
     * They are updated periodically at {@link #refreshIntervalMediumPriority}.
     */
    private final Set<ChannelUID> mediumPriorityChannels = new HashSet<>();

    /**
     * Channels with priority configuration parameter set to Low. They represent static information or information
     * that is updated rare- e.g. CPU name, storage name and etc.
     * They are updated only at {@link #initialize()}.
     */
    private final Set<ChannelUID> lowPriorityChannels = new HashSet<>();

    /**
     * Wait time for the creation of Item-Channel links in seconds. This delay is needed, because the Item-Channel
     * links have to be created before the thing state is updated, otherwise item state will not be updated.
     */
    public static final int WAIT_TIME_CHANNEL_ITEM_LINK_INIT = 1;

    public final SystemInfoThingTypeProvider thingTypeProvider;

    private SystemInfoInterface systeminfo;

    private @Nullable ScheduledFuture<?> highPriorityTasks;
    private @Nullable ScheduledFuture<?> mediumPriorityTasks;

    /**
     * Caches for cpu process load and process load for a given pid. Using this cache limits the process load refresh
     * interval to the minimum interval. Too frequent refreshes leads to inaccurate results. This could happen when the
     * same process is tracked as current process and as a channel with pid parameter, or when the task interval is set
     * too low.
     */
    private static final int MIN_PROCESS_LOAD_REFRESH_INTERVAL_MS = 2000;
    private ExpiringCache<PercentType> cpuLoadCache = new ExpiringCache<>(MIN_PROCESS_LOAD_REFRESH_INTERVAL_MS,
            () -> getSystemCpuLoad());
    private ExpiringCacheMap<Integer, @Nullable DecimalType> processLoadCache = new ExpiringCacheMap<>(
            MIN_PROCESS_LOAD_REFRESH_INTERVAL_MS);

    private final Logger logger = LoggerFactory.getLogger(SystemInfoComputerHandler.class);

    public SystemInfoComputerHandler(Bridge bridge, SystemInfoThingTypeProvider thingTypeProvider,
            SystemInfoInterface systeminfo) {
        super(bridge);
        this.systeminfo = systeminfo;
        this.thingTypeProvider = thingTypeProvider;
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
                    final String priority = (String) properties.get(PRIORITY_PARAMETER);
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
    public void handleRemoval() {
        thingTypeProvider.removeThingType(thing.getThingTypeUID());
        super.handleRemoval();
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
            refreshIntervalMediumPriority = (BigDecimal) configuration.get(MEDIUM_PRIORITY_REFRESH_TIME);
            refreshIntervalHighPriority = (BigDecimal) configuration.get(HIGH_PRIORITY_REFRESH_TIME);

            if (refreshIntervalHighPriority.intValue() <= 0 || refreshIntervalMediumPriority.intValue() <= 0) {
                throw new IllegalArgumentException("Refresh time must be positive number!");
            }
            logger.debug("Refresh time for medium priority channels set to {} s", refreshIntervalMediumPriority);
            logger.debug("Refresh time for high priority channels set to {} s", refreshIntervalHighPriority);
            return true;
        } catch (IllegalArgumentException exception) {
            logger.warn("Refresh time value is invalid! Please change the thing configuration!");
        } catch (ClassCastException exception) {
            logger.debug("Channel configuration cannot be read!");
        }
        return false;
    }

    private boolean updateProperties() {
        Map<String, String> properties = editProperties();
        try {
            properties.put(PROPERTY_CPU_LOGICAL_CORES, systeminfo.getCpuLogicalCores().toString());
            properties.put(PROPERTY_CPU_PHYSICAL_CORES, systeminfo.getCpuPhysicalCores().toString());
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
        addChannelGroups(CHANNEL_GROUP_STORAGE, CHANNEL_GROUP_TYPE_STORAGE, systeminfo.getFileOSStoreCount(),
                newChannelGroups);
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
        addChannels(CHANNEL_CPU_FREQ, systeminfo.getCpuLogicalCores().intValue(), newChannels);
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
        logger.debug("Schedule high priority tasks at fixed rate {} s", refreshIntervalHighPriority);
        highPriorityTasks = scheduler.scheduleWithFixedDelay(() -> {
            publishData(highPriorityChannels);
        }, WAIT_TIME_CHANNEL_ITEM_LINK_INIT, refreshIntervalHighPriority.intValue(), TimeUnit.SECONDS);

        logger.debug("Schedule medium priority tasks at fixed rate {} s", refreshIntervalMediumPriority);
        mediumPriorityTasks = scheduler.scheduleWithFixedDelay(() -> {
            publishData(mediumPriorityChannels);
        }, WAIT_TIME_CHANNEL_ITEM_LINK_INIT, refreshIntervalMediumPriority.intValue(), TimeUnit.SECONDS);

        logger.debug("Schedule one time update for low priority tasks");
        scheduler.schedule(() -> {
            publishData(lowPriorityChannels);
        }, WAIT_TIME_CHANNEL_ITEM_LINK_INIT, TimeUnit.SECONDS);
    }

    private void publishData(Set<ChannelUID> channels) {
        if (ThingStatus.ONLINE.equals(thing.getStatus())) {
            for (final ChannelUID channelUID : channels) {
                final Thing child = getThing().getThing(channelUID.getThingUID());
                if (child != null) {
                    final ThingHandler handler = child.getHandler();
                    if (handler != null) {
                        handler.handleCommand(channelUID, RefreshType.REFRESH);
                    } else {
                        logger.warn("Tried to update channel {} on not configured thing {}", channelUID,
                                child.getUID());
                    }
                } else {
                    handleCommand(channelUID, RefreshType.REFRESH);
                }
            }
        }
    }

    public Set<ChannelUID> getHighPriorityChannels() {
        return highPriorityChannels;
    }

    public Set<ChannelUID> getMediumPriorityChannels() {
        return mediumPriorityChannels;
    }

    public Set<ChannelUID> getLowPriorityChannels() {
        return lowPriorityChannels;
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
        // All digits are deleted from the ID, except for CpuLoad channels.
        if (!(CHANNEL_CPU_LOAD_1.equals(channelID) || CHANNEL_CPU_LOAD_5.equals(channelID)
                || CHANNEL_CPU_LOAD_15.equals(channelID))) {
            channelID = channelID.replaceAll("\\d+", "");
        }

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
                case CHANNEL_SENSORS_CPU_TEMPERATURE:
                    state = systeminfo.getSensorsCpuTemperature();
                    break;
                case CHANNEL_SENOSRS_CPU_VOLTAGE:
                    state = systeminfo.getSensorsCpuVoltage();
                    break;
                case CHANNEL_SENSORS_FAN_SPEED:
                    state = systeminfo.getSensorsFanSpeed(deviceIndex);
                    break;
                case CHANNEL_CPU_MAXFREQ:
                    state = systeminfo.getCpuMaxFreq();
                    break;
                case CHANNEL_CPU_FREQ:
                    state = systeminfo.getCpuFreq(deviceIndex);
                    break;
                case CHANNEL_CPU_LOAD:
                    PercentType cpuLoad = cpuLoadCache.getValue();
                    state = (cpuLoad != null) ? new QuantityType<>(cpuLoad, Units.PERCENT) : null;
                    break;
                case CHANNEL_CPU_LOAD_1:
                    state = systeminfo.getCpuLoad1();
                    break;
                case CHANNEL_CPU_LOAD_5:
                    state = systeminfo.getCpuLoad5();
                    break;
                case CHANNEL_CPU_LOAD_15:
                    state = systeminfo.getCpuLoad15();
                    break;
                case CHANNEL_CPU_UPTIME:
                    state = systeminfo.getCpuUptime();
                    break;
                case CHANNEL_CPU_THREADS:
                    state = systeminfo.getCpuThreads();
                    break;
                case CHANNEL_CPU_DESCRIPTION:
                    state = systeminfo.getCpuDescription();
                    break;
                case CHANNEL_CPU_NAME:
                    state = systeminfo.getCpuName();
                    break;
                case CHANNEL_STORAGE_NAME:
                    state = systeminfo.getStorageName(deviceIndex);
                    break;
                case CHANNEL_STORAGE_DESCRIPTION:
                    state = systeminfo.getStorageDescription(deviceIndex);
                    break;
                case CHANNEL_STORAGE_AVAILABLE:
                    state = systeminfo.getStorageAvailable(deviceIndex);
                    break;
                case CHANNEL_STORAGE_USED:
                    state = systeminfo.getStorageUsed(deviceIndex);
                    break;
                case CHANNEL_STORAGE_TOTAL:
                    state = systeminfo.getStorageTotal(deviceIndex);
                    break;
                case CHANNEL_STORAGE_TYPE:
                    state = systeminfo.getStorageType(deviceIndex);
                    break;
                case CHANNEL_STORAGE_AVAILABLE_PERCENT:
                    PercentType storageAvailablePercent = systeminfo.getStorageAvailablePercent(deviceIndex);
                    state = (storageAvailablePercent != null)
                            ? new QuantityType<>(storageAvailablePercent, Units.PERCENT)
                            : null;
                    break;
                case CHANNEL_STORAGE_USED_PERCENT:
                    PercentType storageUsedPercent = systeminfo.getStorageUsedPercent(deviceIndex);
                    state = (storageUsedPercent != null) ? new QuantityType<>(storageUsedPercent, Units.PERCENT) : null;
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
                default:
                    logger.debug("Channel with unknown ID: {} !", channelID);
            }
        } catch (DeviceNotFoundException e) {
            logger.warn("No information for channel {} with device index: {}", channelID, deviceIndex);
        } catch (Exception e) {
            logger.debug("Unexpected error occurred while getting system information!", e);
            updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.COMMUNICATION_ERROR, "@text/offline.unexpected-error");
        }
        return state != null ? state : UnDefType.UNDEF;
    }

    private @Nullable PercentType getSystemCpuLoad() {
        return systeminfo.getSystemCpuLoad();
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

    @Override
    public void handleCommand(ChannelUID channelUID, Command command) {
        if (thing.getStatus().equals(ThingStatus.ONLINE)) {
            if (command instanceof RefreshType) {
                logger.debug("Refresh command received for channel {} !", channelUID);
                if (CHANNEL_MEMORY_GROUP.equals(channelUID.getGroupId())) {
                    final GlobalMemory memory = systeminfo.getMemorySpecifications();
                    updateState(channelUID, getChannelState(channelUID, memory.getAvailable(),
                            memory.getTotal() - memory.getAvailable(), memory.getTotal()));
                } else if (CHANNEL_HEAP_GROUP.equals(channelUID.getGroupId())) {
                    final Runtime runtime = Runtime.getRuntime();
                    updateState(channelUID, getChannelState(channelUID, runtime.freeMemory(),
                            runtime.totalMemory() - runtime.freeMemory(), runtime.totalMemory()));
                } else if (CHANNEL_SWAP_GROUP.equals(channelUID.getGroupId())) {
                    final VirtualMemory swap = systeminfo.getSwapSpecifications();
                    updateState(channelUID, getChannelState(channelUID, swap.getSwapTotal() - swap.getSwapUsed(),
                            swap.getSwapUsed(), swap.getSwapTotal()));
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

    private boolean isConfigurationKeyChanged(Configuration currentConfig, Configuration newConfig, String key) {
        Object currentValue = currentConfig.get(key);
        Object newValue = newConfig.get(key);

        if (currentValue == null) {
            return (newValue != null);
        }

        return !currentValue.equals(newValue);
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

                String newPriority = (String) newChannelConfig.get(PRIORITY_PARAMETER);
                changeChannelPriority(channelUID, newPriority);
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

    public void changeChannelPriority(final ChannelUID channelUID, final String priority) {
        mediumPriorityChannels.remove(channelUID);
        lowPriorityChannels.remove(channelUID);
        highPriorityChannels.remove(channelUID);
        switch (priority) {
            case "High": {
                highPriorityChannels.add(channelUID);
                break;
            }
            case "Medium": {
                mediumPriorityChannels.add(channelUID);
                break;
            }
            case "Low": {
                lowPriorityChannels.add(channelUID);
                break;
            }
            default: {
                logger.debug("Invalid priority configuration parameter. Channel will not be updated!");
            }
        }
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

    private State getChannelState(final ChannelUID channelUID, long available, long used, long total) {
        return (total <= 0) || (total < available) || (total < used) ? UnDefType.UNDEF
                : switch (channelUID.getIdWithoutGroup()) {
                    case CHANNEL_AVAILABLE -> new QuantityType<>(available, Units.BYTE);
                    case CHANNEL_AVAILABLE_PERCENT ->
                        new QuantityType<>(round((double) available / (double) total), Units.PERCENT);
                    case CHANNEL_TOTAL -> new QuantityType<>(total, Units.BYTE);
                    case CHANNEL_USED -> new QuantityType<>(used, Units.BYTE);
                    case CHANNEL_USED_PERCENT ->
                        new QuantityType<>(round((double) used / (double) total), Units.PERCENT);
                    default -> UnDefType.UNDEF;
                };
    }

    private BigDecimal round(final double quotient) {
        final BigDecimal result = BigDecimal.valueOf(quotient * 100.0);
        return result.setScale(PRECISION_AFTER_DECIMAL_SIGN, RoundingMode.HALF_UP);
    }
}
