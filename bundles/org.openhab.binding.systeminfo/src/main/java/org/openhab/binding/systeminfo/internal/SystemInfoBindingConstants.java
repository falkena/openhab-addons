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
    public static final ThingTypeUID THING_TYPE_PARTITION = new ThingTypeUID(BINDING_ID, "partition");

    // Thing properties
    /**
     * Number of CPU logical cores
     */
    public static final String PROPERTY_CPU_LOGICAL_CORES = "CPU Logical Cores";

    /**
     * Number of CPU physical cores
     */
    public static final String PROPERTY_CPU_PHYSICAL_CORES = "CPU Physical Cores";

    public static final String PROPERTY_IDENTIFICATION = "identification";

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
     * Type of the logical device, is used in partition and volume
     */
    public static final String CHANNEL_TYPE = "type";

    /**
     * Size of the available memory, is used in physical, heap and swap memory channels
     */
    public static final String CHANNEL_AVAILABLE = "available";

    /**
     * Percents of the available memory, is used in physical, heap and swap memory channels
     */
    public static final String CHANNEL_AVAILABLE_PERCENT = "availablePercent";

    /**
     * Total size of memory, is used in physical, heap and swap memory channels
     */
    public static final String CHANNEL_TOTAL = "total";

    /**
     * Size of the used memory, is used in physical, heap and swap memory channels
     */
    public static final String CHANNEL_USED = "used";

    /**
     * Percents of the used memory, is used in physical, heap and swap memory channels
     */
    public static final String CHANNEL_USED_PERCENT = "usedPercent";

    /**
     * Frequency of the CPU
     */
    public static final String CHANNEL_CPU_FREQUENCY = "frequency";

    /**
     * Maximum frequency of the CPU
     */
    public static final String CHANNEL_CPU_MAXFREQUENCY = "maxFrequency";

    /**
     * Temperature of the CPU measured from the sensors.
     */
    public static final String CHANNEL_CPU_TEMPERATURE = "temperature";

    /**
     * Voltage of the CPU core.
     */
    public static final String CHANNEL_CPU_VOLTAGE = "voltage";

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
     * Average recent system load
     */
    public static final String CHANNEL_SYSTEM_LOAD = "load";

    /**
     * Average system load for the last minute
     */
    public static final String CHANNEL_SYSTEM_LOAD_1 = "load1";

    /**
     * Average system load for the last 5 minutes
     */
    public static final String CHANNEL_SYSTEM_LOAD_5 = "load5";

    /**
     * Average system load for the last 15 minutes
     */
    public static final String CHANNEL_SYSTEM_LOAD_15 = "load15";

    /**
     * System running threads count
     */
    public static final String CHANNEL_SYSTEM_THREADS = "threads";

    /**
     * System uptime in minutes
     */
    public static final String CHANNEL_SYSTEM_UPTIME = "uptime";

    /**
     * Name of the channel group for CPU information
     */
    public static final String CHANNEL_CPU_GROUP = "cpu";

    /**
     * Name of the channel group for heap information
     */
    public static final String CHANNEL_HEAP_GROUP = "heap";

    /**
     * Name of the channel group for memory information
     */
    public static final String CHANNEL_MEMORY_GROUP = "memory";

    /**
     * Name of the channel group type for partition information
     */
    public static final String CHANNEL_PARTITION_GROUP = "partition";

    /**
     * Name of the channel group for CPU information
     */
    public static final String CHANNEL_SYSTEM_GROUP = "system";

    /**
     * Name of the channel group type for volume information
     */
    public static final String CHANNEL_VOLUME_GROUP = "volume";

    /**
     * Name of the channel group for swap information
     */
    public static final String CHANNEL_SWAP_GROUP = "swap";

    // Old definitions
    /**
     * Name of the channel group type for sensors information
     */
    public static final String CHANNEL_GROUP_TYPE_SENSORS = "sensorsGroup";

    /**
     * Name of the channel group for sensors information
     */
    public static final String CHANNEL_GROUP_SENSORS = "sensors";

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

    public static final int PRECISION_AFTER_DECIMAL_SIGN = 1;
}
