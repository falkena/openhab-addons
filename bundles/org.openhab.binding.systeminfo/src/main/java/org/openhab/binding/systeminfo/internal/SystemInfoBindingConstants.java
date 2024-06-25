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
package org.openhab.binding.systeminfo.internal;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.core.thing.ThingTypeUID;

/**
 * The {@link SystemInfoBindingConstants} class defines common constants, which are
 * used across the whole binding.
 *
 * @author Svilen Valkanov - Initial contribution
 * @author Mark Herwege - Add dynamic creation of extra channels
 * @author Mark Herwege - Processor frequency channels
 */
@NonNullByDefault
public class SystemInfoBindingConstants {

    public static final String BINDING_ID = "systeminfo";

    public static final String BRIDGE_TYPE_COMPUTER_ID = "computer";
    public static final ThingTypeUID BRIDGE_TYPE_COMPUTER = new ThingTypeUID(BINDING_ID, BRIDGE_TYPE_COMPUTER_ID);
    public static final ThingTypeUID BRIDGE_TYPE_COMPUTER_IMPL = new ThingTypeUID(BINDING_ID,
            BRIDGE_TYPE_COMPUTER_ID + "-impl");

    public static final ThingTypeUID BRIDGE_TYPE_DRIVE = new ThingTypeUID(BINDING_ID, "drive");
    public static final ThingTypeUID THING_TYPE_NETWORK = new ThingTypeUID(BINDING_ID, "network");

    // Thing properties
    /**
     * Number of CPU logical cores
     */
    public static final String PROPERTY_CPU_LOGICAL_CORES = "CPU Logical Cores";

    /**
     * Number of CPU physical cores
     */
    public static final String PROPERTY_CPU_PHYSICAL_CORES = "CPU Physical Cores";

    public static final String PROPERTY_HOSTNAME = "hostname";
    public static final String PROPERTY_HOSTNAME_DEFAULT = "unknown";

    /**
     * Contains information about the family /Windows, Linux, OS X etc/ of the operation system
     */
    public static final String PROPERTY_OS_FAMILY = "OS Family";

    /**
     * Name of the manufacturer of the operation system
     */
    public static final String PROPERTY_OS_MANUFACTURER = "OS Manufacturer";

    /**
     * Version of the operation system
     */
    public static final String PROPERTY_OS_VERSION = "OS Version";

    // List of all Channel IDs

    /**
     * Common description channel
     */
    public static final String CHANNEL_DESCRIPTION = "description";

    /**
     * Common name channel
     */
    public static final String CHANNEL_NAME = "name";

    /**
     * Physical storage drive model
     */
    public static final String CHANNEL_DRIVE_MODEL = "model";

    /**
     * Physical storage drive serial number
     */
    public static final String CHANNEL_DRIVE_SERIAL = "serial";

    /**
     * The number of reads from the disk
     */
    public static final String CHANNEL_DRIVE_READS = "reads";

    /**
     * The number of bytes read from the disk
     */
    public static final String CHANNEL_DRIVE_READ_BYTES = "readBytes";

    /**
     * The number of writes to the disk
     */
    public static final String CHANNEL_DRIVE_WRITES = "writes";

    /**
     * The number of bytes written to the disk
     */
    public static final String CHANNEL_DRIVE_WRITE_BYTES = "writeBytes";

    /**
     * Host IP address of the network interface
     */
    public static final String CHANNEL_NETWORK_IP = "ip";

    /**
     * MAC address of the network interface
     */
    public static final String CHANNEL_NETWORK_MAC = "mac";

    /**
     * Number of packets received over network interface
     */
    public static final String CHANNEL_NETWORK_RECEIVED = "received";

    /**
     * The number of bytes received over network interface
     */
    public static final String CHANNEL_NETWORK_RECEIVED_BYTES = "receivedBytes";

    /**
     * Number of packets sent over network interface
     */
    public static final String CHANNEL_NETWORK_SENT = "sent";

    /**
     * The number of bytes sent over network interface
     */
    public static final String CHANNEL_NETWORK_SENT_BYTES = "sentBytes";

    /**
     * Name of the channel group type for memory information
     */
    public static final String CHANNEL_GROUP_TYPE_MEMORY = "memoryGroup";

    /**
     * Name of the channel group for memory information
     */
    public static final String CHANNEL_GROUP_MEMORY = "memory";

    /**
     * Size of the available memory
     */
    public static final String CHANNEL_MEMORY_AVAILABLE = "memory#available";

    /**
     * Size of the used memory
     */
    public static final String CHANNEL_MEMORY_USED = "memory#used";

    /**
     * Total size of the memory
     */
    public static final String CHANNEL_MEMORY_TOTAL = "memory#total";

    /**
     * Percents of the available memory
     */
    public static final String CHANNEL_MEMORY_AVAILABLE_PERCENT = "memory#availablePercent";

    /**
     * Percents of the used memory
     */
    public static final String CHANNEL_MEMORY_USED_PERCENT = "memory#usedPercent";

    /**
     * Percents of the used heap
     */
    public static final String CHANNEL_MEMORY_USED_HEAP_PERCENT = "memory#usedHeapPercent";

    /**
     * Bytes used in the heap
     */
    public static final String CHANNEL_MEMORY_HEAP_AVAILABLE = "memory#availableHeap";

    /**
     * Name of the channel group type for swap information
     */
    public static final String CHANNEL_GROUP_TYPE_SWAP = "swapGroup";

    /**
     * Name of the channel group for swap information
     */
    public static final String CHANNEL_GROUP_SWAP = "swap";

    /**
     * Total size of swap memory
     */
    public static final String CHANNEL_SWAP_TOTAL = "swap#total";

    /**
     * Size of the available swap memory
     */
    public static final String CHANNEL_SWAP_AVAILABLE = "swap#available";

    /**
     * Size of the used swap memory
     */
    public static final String CHANNEL_SWAP_USED = "swap#used";

    /**
     * Percents of the available swap memory
     */
    public static final String CHANNEL_SWAP_AVAILABLE_PERCENT = "swap#availablePercent";

    /**
     * Percents of the used swap memory
     */
    public static final String CHANNEL_SWAP_USED_PERCENT = "swap#usedPercent";

    /**
     * Name of the channel group type for storage information
     */
    public static final String CHANNEL_GROUP_TYPE_STORAGE = "storageGroup";

    /**
     * Name of the channel group for storage information
     */
    public static final String CHANNEL_GROUP_STORAGE = "storage";

    /**
     * Name of the logical volume storage
     */
    public static final String CHANNEL_STORAGE_NAME = "storage#name";

    /**
     * Logical storage volume type -(e.g. NTFS, FAT32 ..)
     */
    public static final String CHANNEL_STORAGE_TYPE = "storage#type";

    /**
     * Description of the logical volume storage
     */
    public static final String CHANNEL_STORAGE_DESCRIPTION = "storage#description";

    /**
     * Size of the available storage space
     */
    public static final String CHANNEL_STORAGE_AVAILABLE = "storage#available";

    /**
     * Size of the used storage space
     */
    public static final String CHANNEL_STORAGE_USED = "storage#used";

    /**
     * Total storage space
     */
    public static final String CHANNEL_STORAGE_TOTAL = "storage#total";

    /**
     * Percents of the available storage space
     */
    public static final String CHANNEL_STORAGE_AVAILABLE_PERCENT = "storage#availablePercent";

    /**
     * Percents of the used storage space
     */
    public static final String CHANNEL_STORAGE_USED_PERCENT = "storage#usedPercent";

    /**
     * Name of the channel group type for sensors information
     */
    public static final String CHANNEL_GROUP_TYPE_SENSORS = "sensorsGroup";

    /**
     * Name of the channel group for sensors information
     */
    public static final String CHANNEL_GROUP_SENSORS = "sensors";

    /**
     * Temperature of the CPU measured from the sensors.
     */
    public static final String CHANNEL_SENSORS_CPU_TEMPERATURE = "sensors#cpuTemp";

    /**
     * Voltage of the CPU core.
     */
    public static final String CHANNEL_SENOSRS_CPU_VOLTAGE = "sensors#cpuVoltage";

    /**
     * Fan speed
     */
    public static final String CHANNEL_SENSORS_FAN_SPEED = "sensors#fanSpeed";

    /**
     * Name of the channel group type for battery information
     */
    public static final String CHANNEL_GROUP_TYPE_BATTERY = "batteryGroup";

    /**
     * Name of the channel group for battery information
     */
    public static final String CHANNEL_GROUP_BATTERY = "battery";

    /**
     * Name of the battery
     */
    public static final String CHANNEL_BATTERY_NAME = "battery#name";

    /**
     * Remaining capacity of the battery.
     */
    public static final String CHANNEL_BATTERY_REMAINING_CAPACITY = "battery#remainingCapacity";

    /**
     * Estimated remaining time of the battery
     */
    public static final String CHANNEL_BATTERY_REMAINING_TIME = "battery#remainingTime";

    /**
     * Name of the channel group type for CPU information
     */
    public static final String CHANNEL_GROUP_TYPE_CPU = "cpuGroup";

    /**
     * Name of the channel group for CPU information
     */
    public static final String CHANNEL_GROUP_CPU = "cpu";

    /**
     * Detailed description about the CPU
     */
    public static final String CHANNEL_CPU_DESCRIPTION = "cpu#description";

    /**
     * Maximum frequency of the CPU
     */
    public static final String CHANNEL_CPU_MAXFREQ = "cpu#maxfreq";

    /**
     * Frequency of the CPU
     */
    public static final String CHANNEL_CPU_FREQ = "cpu#freq";

    /**
     * Average recent CPU load
     */
    public static final String CHANNEL_CPU_LOAD = "cpu#load";

    /**
     * Average CPU load for the last minute
     */
    public static final String CHANNEL_CPU_LOAD_1 = "cpu#load1";

    /**
     * Average CPU load for the last 5 minutes
     */
    public static final String CHANNEL_CPU_LOAD_5 = "cpu#load5";

    /**
     * Average CPU load for the last 15 minutes
     */
    public static final String CHANNEL_CPU_LOAD_15 = "cpu#load15";

    /**
     * CPU name
     */
    public static final String CHANNEL_CPU_NAME = "cpu#name";

    /**
     * CPU uptime in minutes
     */
    public static final String CHANNEL_CPU_UPTIME = "cpu#uptime";

    /**
     * CPU running threads count
     */
    public static final String CHANNEL_CPU_THREADS = "cpu#threads";

    /**
     * Name of the channel group type for display information
     */
    public static final String CHANNEL_GROUP_TYPE_DISPLAY = "displayGroup";

    /**
     * Name of the channel group for display information
     */
    public static final String CHANNEL_GROUP_DISPLAY = "display";

    /**
     * Information about the display device
     */
    public static final String CHANNEL_DISPLAY_INFORMATION = "display#information";

    /**
     * Name of the channel group type for process information
     */
    public static final String CHANNEL_GROUP_TYPE_CURRENT_PROCESS = "currentProcessGroup";

    /**
     * Name of the channel group for process information
     */
    public static final String CHANNEL_GROUP_CURRENT_PROCESS = "currentProcess";

    /**
     * CPU load used from a process
     */

    public static final String CHANNEL_CURRENT_PROCESS_LOAD = "currentProcess#load";

    /**
     * Size of memory used from a process in MB
     */
    public static final String CHANNEL_CURRENT_PROCESS_MEMORY = "currentProcess#used";

    /**
     * Name of the process
     */
    public static final String CHANNEL_CURRENT_PROCESS_NAME = "currentProcess#name";

    /**
     * Number of threads, used form the process
     */
    public static final String CHANNEL_CURRENT_PROCESS_THREADS = "currentProcess#threads";

    /**
     * The full path of the process
     */
    public static final String CHANNEL_CURRENT_PROCESS_PATH = "currentProcess#path";

    /**
     * Name of the channel group type for process information
     */
    public static final String CHANNEL_GROUP_TYPE_PROCESS = "processGroup";

    /**
     * Name of the channel group for process information
     */
    public static final String CHANNEL_GROUP_PROCESS = "process";

    /**
     * CPU load used from a process
     */

    public static final String CHANNEL_PROCESS_LOAD = "process#load";

    /**
     * Size of memory used from a process in MB
     */
    public static final String CHANNEL_PROCESS_MEMORY = "process#used";

    /**
     * Name of the process
     */
    public static final String CHANNEL_PROCESS_NAME = "process#name";

    /**
     * Number of threads, used form the process
     */
    public static final String CHANNEL_PROCESS_THREADS = "process#threads";

    /**
     * The full path of the process
     */
    public static final String CHANNEL_PROCESS_PATH = "process#path";

    // Thing configuraion
    /**
     * Name of the configuration parameter of the thing that defines refresh time for Low priority channels
     */
    public static final String LOW_PRIORITY_REFRESH_TIME = "interval_low";

    /**
     * Name of the configuration parameter of the thing that defines refresh time for Medium priority channels
     */
    public static final String MEDIUM_PRIORITY_REFRESH_TIME = "interval_medium";

    /**
     * Name of the configuration parameter of the thing that defines refresh time for High priority channels
     */
    public static final String HIGH_PRIORITY_REFRESH_TIME = "interval_high";

    public static final String DEVICE_INDEX_PARAMETER = "index";

    public static final String DEVICE_NAME_PARAMETER = "name";

    // Channel configuration

    /**
     * Name of the channel configuration parameter priority
     */
    public static final String PRIORITY_PARAMETER = "priority";

    /**
     * Name of the channel configuration parameter pid
     *
     */
    public static final String PID_PARAMETER = "pid";
}
