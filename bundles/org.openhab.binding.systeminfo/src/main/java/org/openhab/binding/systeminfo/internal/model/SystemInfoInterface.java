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
package org.openhab.binding.systeminfo.internal.model;

import java.util.List;

import javax.measure.quantity.Time;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.core.library.dimension.DataAmount;
import org.openhab.core.library.types.DecimalType;
import org.openhab.core.library.types.PercentType;
import org.openhab.core.library.types.QuantityType;
import org.openhab.core.library.types.StringType;

import oshi.hardware.CentralProcessor;
import oshi.hardware.ComputerSystem;
import oshi.hardware.GlobalMemory;
import oshi.hardware.HWDiskStore;
import oshi.hardware.NetworkIF;
import oshi.hardware.Sensors;
import oshi.hardware.VirtualMemory;
import oshi.software.os.OSFileStore;
import oshi.software.os.OperatingSystem;

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
     * Returns the CPU specification.
     */
    CentralProcessor getCPUSpecification();

    /**
     * Returns the CPU identifier.
     */
    default CentralProcessor.ProcessorIdentifier getCPUIdentifier() {
        return getCPUSpecification().getProcessorIdentifier();
    }

    // File storage info

    /**
     * Returns the number of file storages.
     *
     * @return file storage count
     */
    default int getFileStorageCount() {
        return getFileStorageList().size();
    }

    /**
     * Gets the list of the file storages
     */
    List<OSFileStore> getFileStorageList();

    // Hardware drive info

    /**
     * Returns the number of drives.
     *
     * @return drive count
     */
    default int getHardDriveCount() {
        return getHardDriveList().size();
    }

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
    default int getNetworkInterfaceCount() {
        return getNetworkInterfaceList().size();
    }

    /**
     * Gets the list of the network interfaces
     */
    List<NetworkIF> getNetworkInterfaceList();

    // System info

    /**
     * Gets the information about operating system
     */
    OperatingSystem getOperatingSystem();

    /**
     * Gets the system sensors
     */
    Sensors getSensors();

    /**
     * Gets the basic information about system
     */
    ComputerSystem getSystem();

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
