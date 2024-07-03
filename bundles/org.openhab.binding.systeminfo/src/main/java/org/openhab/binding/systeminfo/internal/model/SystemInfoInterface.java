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
package org.openhab.binding.systeminfo.internal.model;

import java.util.List;

import javax.measure.quantity.ElectricPotential;
import javax.measure.quantity.Frequency;
import javax.measure.quantity.Temperature;
import javax.measure.quantity.Time;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.core.library.dimension.DataAmount;
import org.openhab.core.library.types.DecimalType;
import org.openhab.core.library.types.PercentType;
import org.openhab.core.library.types.QuantityType;
import org.openhab.core.library.types.StringType;

import oshi.hardware.GlobalMemory;
import oshi.hardware.HWDiskStore;
import oshi.hardware.NetworkIF;
import oshi.hardware.VirtualMemory;

/**
 * {@link SystemInfoInterface} defines the methods needed to provide this binding with the required system information.
 *
 * @author Svilen Valkanov - Initial contribution
 * @author Wouter Born - Add null annotations
 * @author Mark Herwege - Add dynamic creation of extra channels
 * @author Mark Herwege - Use units of measure
 * @author Mark Herwege - Processor frequency channels
 */
@NonNullByDefault
public interface SystemInfoInterface {

    /**
     * Initialize logic for the SystemInfo implementation
     */
    void initializeSystemInfo();

    // Operating system info
    /**
     * Get the Family of the operating system /e.g. Windows, Unix,.../
     */
    StringType getOsFamily();

    /**
     * Get the manufacturer of the operating system
     */
    StringType getOsManufacturer();

    /**
     * Get the version of the operating system
     *
     * @return
     */
    StringType getOsVersion();

    // CPU info
    /**
     * Get the name of the CPU
     */
    StringType getCpuName();

    /**
     * Get description about the CPU e.g (model, family, vendor, serial number, identifier, architecture(32bit or
     * 64bit))
     */
    StringType getCpuDescription();

    /**
     * Get the number of logical CPUs/cores available for processing.
     */
    DecimalType getCpuLogicalCores();

    /**
     * Get the number of physical CPUs/cores available for processing.
     */
    DecimalType getCpuPhysicalCores();

    /**
     * Get the maximum CPU frequency of the processor.
     */
    @Nullable
    QuantityType<Frequency> getCpuMaxFreq();

    /**
     * Get the current CPU frequency of a logical processor.
     */
    @Nullable
    QuantityType<Frequency> getCpuFreq(int logicalProcessorIndex);

    /**
     * Returns the system cpu load.
     *
     * @return the system cpu load between 0 and 100% or null, if no information is available
     */
    @Nullable
    PercentType getSystemCpuLoad();

    /**
     * Returns the system load average for the last minute.
     *
     * @return the load as a number of processes or null, if no information is available
     */
    @Nullable
    DecimalType getCpuLoad1();

    /**
     * Returns the system load average for the last 5 minutes.
     *
     * @return the load as number of processes or null, if no information is available
     */
    @Nullable
    DecimalType getCpuLoad5();

    /**
     * Returns the system load average for the last 15 minutes.
     *
     * @return the load as number of processes or null, if no information is available
     */
    @Nullable
    DecimalType getCpuLoad15();

    /**
     * Get the System uptime (time since boot).
     *
     * @return time since boot
     */
    QuantityType<Time> getCpuUptime();

    /**
     * Get the number of threads currently running
     *
     * @return number of threads
     */
    DecimalType getCpuThreads();

    // Storage info
    /**
     * Returns the total space of the logical storage volume.
     *
     * @param deviceIndex - the index of the logical volume
     * @return storage size
     * @throws DeviceNotFoundException
     */
    QuantityType<DataAmount> getStorageTotal(int deviceIndex) throws DeviceNotFoundException;

    /**
     * Returns the available storage space on the logical storage volume
     *
     * @param deviceIndex - the index of the logical volume
     * @return storage size
     * @throws DeviceNotFoundException
     */
    QuantityType<DataAmount> getStorageAvailable(int deviceIndex) throws DeviceNotFoundException;

    /**
     * Gets the used storage space on the logical storage volume
     *
     * @param deviceIndex - the index of the logical volume
     * @return storage size
     * @throws DeviceNotFoundException
     */
    QuantityType<DataAmount> getStorageUsed(int deviceIndex) throws DeviceNotFoundException;

    /**
     * Gets the percent of available storage on the logical volume
     *
     * @param deviceIndex - the index of the logical volume
     * @return percent of available storage or null
     * @throws DeviceNotFoundException
     */
    @Nullable
    PercentType getStorageAvailablePercent(int deviceIndex) throws DeviceNotFoundException;

    /**
     * Gets the percent of used storage on the logical volume
     *
     * @param deviceIndex - the index of the logical volume
     * @return percent of used storage or null
     * @throws DeviceNotFoundException
     */
    @Nullable
    PercentType getStorageUsedPercent(int deviceIndex) throws DeviceNotFoundException;

    /**
     * Gets the name of the logical storage volume
     *
     * @throws DeviceNotFoundException
     */
    StringType getStorageName(int deviceIndex) throws DeviceNotFoundException;

    /**
     * Gets the type of the logical storage volume (e.g. NTFS, FAT32)
     *
     * @throws DeviceNotFoundException
     */
    StringType getStorageType(int deviceIndex) throws DeviceNotFoundException;

    /**
     * Gets the description of the logical storage volume
     *
     * @throws DeviceNotFoundException
     */
    StringType getStorageDescription(int deviceIndex) throws DeviceNotFoundException;

    // Hardware drive info

    /**
     * Returns the number of drives.
     *
     * @return drive count
     */
    int getHardDriveCount();

    /**
     * Gets the list of the physical storage drives
     */
    List<HWDiskStore> getHardDriveList();

    // Memory info

    /**
     * Returns the memory parameter and information.
     *
     * @return memory information
     */
    GlobalMemory getMemorySpecifications();

    /**
     * Returns the swap parameter and information.
     *
     * @return swap information
     */
    VirtualMemory getSwapSpecifications();

    // Network info

    /**
     * Returns the number of network interfaces.
     *
     * @return network interface count
     */
    int getNetworkInterfaceCount();

    /**
     * Gets the list of the network interfaces
     */
    List<NetworkIF> getNetworkInterfaceList();

    // Display info
    /**
     * Get information about the display device as product number, manufacturer, serial number, width and height in cm";
     *
     * @param deviceIndex - the index of the display device
     * @throws DeviceNotFoundException
     */
    StringType getDisplayInformation(int deviceIndex) throws DeviceNotFoundException;

    // Sensors info
    /**
     * Get the information from the CPU temperature sensors.
     *
     * @return Temperature if available, null otherwise.
     */
    @Nullable
    QuantityType<Temperature> getSensorsCpuTemperature();

    /**
     * Get the information for the CPU voltage.
     *
     * @return Voltage if available, null otherwise.
     */
    @Nullable
    QuantityType<ElectricPotential> getSensorsCpuVoltage();

    /**
     * Get fan speed
     *
     * @param deviceIndex
     * @return Speed in rpm or null if unable to measure fan speed
     * @throws DeviceNotFoundException
     */
    @Nullable
    DecimalType getSensorsFanSpeed(int deviceIndex) throws DeviceNotFoundException;

    // Battery info
    /**
     * Get estimated time remaining for the power source.
     *
     * @param deviceIndex
     * @return duration remaining charge or null, if the time is estimated as unlimited
     * @throws DeviceNotFoundException
     */
    @Nullable
    QuantityType<Time> getBatteryRemainingTime(int deviceIndex) throws DeviceNotFoundException;

    /**
     * Battery remaining capacity.
     *
     * @param deviceIndex
     * @return percentage value
     * @throws DeviceNotFoundException
     */
    PercentType getBatteryRemainingCapacity(int deviceIndex) throws DeviceNotFoundException;

    /**
     * Get battery name
     *
     * @param deviceIndex
     * @throws DeviceNotFoundException
     */
    StringType getBatteryName(int deviceIndex) throws DeviceNotFoundException;

    /**
     * Get PID of process executing this code
     *
     * @return current process ID
     */
    int getCurrentProcessID();

    /**
     * Returns the name of the process
     *
     * @param pid - the PID of the process
     * @throws DeviceNotFoundException - thrown if process with this PID can not be found
     */
    @Nullable
    StringType getProcessName(int pid) throws DeviceNotFoundException;

    /**
     * Returns the CPU usage of the process
     *
     * @param pid - the PID of the process
     * @return - percentage value, can be above 100% if process uses multiple cores
     * @throws DeviceNotFoundException - thrown if process with this PID can not be found
     */
    @Nullable
    DecimalType getProcessCpuUsage(int pid) throws DeviceNotFoundException;

    /**
     * Returns the size of RAM memory only usage of the process
     *
     * @param pid - the PID of the process
     * @return memory size
     * @throws DeviceNotFoundException thrown if process with this PID can not be found
     */
    @Nullable
    QuantityType<DataAmount> getProcessMemoryUsage(int pid) throws DeviceNotFoundException;

    /**
     * Returns the full path of the executing process.
     *
     * @param pid - the PID of the process
     * @throws DeviceNotFoundException - thrown if process with this PID can not be found
     */
    @Nullable
    StringType getProcessPath(int pid) throws DeviceNotFoundException;

    /**
     * Returns the number of threads in this process.
     *
     * @param pid - the PID of the process
     * @throws DeviceNotFoundException - thrown if process with this PID can not be found
     */
    @Nullable
    DecimalType getProcessThreads(int pid) throws DeviceNotFoundException;

    /**
     * Returns the number of displays.
     *
     * @return display count
     */
    int getDisplayCount();

    /**
     * Returns the number of storages.
     *
     * @return storage count
     */
    int getFileOSStoreCount();

    /**
     * Returns the number of power sources/batteries.
     *
     * @return power source count
     */
    int getPowerSourceCount();

    /**
     * Returns the number of fans.
     *
     * @return fan count
     */
    int getFanCount();
}
