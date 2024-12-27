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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.quantity.Time;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.core.library.dimension.DataAmount;
import org.openhab.core.library.types.DecimalType;
import org.openhab.core.library.types.PercentType;
import org.openhab.core.library.types.QuantityType;
import org.openhab.core.library.types.StringType;
import org.openhab.core.library.unit.Units;
import org.osgi.service.component.annotations.Component;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import oshi.SystemInfo;
import oshi.hardware.CentralProcessor;
import oshi.hardware.ComputerSystem;
import oshi.hardware.Display;
import oshi.hardware.GlobalMemory;
import oshi.hardware.HWDiskStore;
import oshi.hardware.HardwareAbstractionLayer;
import oshi.hardware.NetworkIF;
import oshi.hardware.PowerSource;
import oshi.hardware.Sensors;
import oshi.hardware.VirtualMemory;
import oshi.software.os.OSFileStore;
import oshi.software.os.OSProcess;
import oshi.software.os.OperatingSystem;
import oshi.util.EdidUtil;

/**
 * This implementation of {@link SystemInfoInterface} is using the open source library OSHI to provide system
 * information. OSHI is a free JNA-based (native) Operating System and Hardware Information library for Java.
 *
 * @author Svilen Valkanov - Initial contribution
 * @author Lyubomir Papazov - Move the initialization logic that could potentially take long time to the
 *         initializeSystemInfo method
 * @author Christoph Weitkamp - Update to OSHI 3.13.0 - Replaced deprecated method
 *         CentralProcessor#getSystemSerialNumber()
 * @author Wouter Born - Update to OSHI 4.0.0 and add null annotations
 * @author Mark Herwege - Add dynamic creation of extra channels
 * @author Mark Herwege - Use units of measure
 * @author Mark Herwege - Processor frequency channels
 *
 * @see <a href="https://github.com/oshi/oshi">OSHI GitHub repository</a>
 */
@NonNullByDefault
@Component(service = SystemInfoInterface.class)
public class OSHISystemInfo implements SystemInfoInterface {

    private final Logger logger = LoggerFactory.getLogger(OSHISystemInfo.class);

    private @NonNullByDefault({}) HardwareAbstractionLayer hal;

    // Dynamic objects (may be queried repeatedly)
    private @NonNullByDefault({}) GlobalMemory memory;
    private @NonNullByDefault({}) CentralProcessor cpu;
    private @NonNullByDefault({}) Sensors sensors;

    // Static objects, should be recreated on each request
    private @NonNullByDefault({}) ComputerSystem computerSystem;
    private @NonNullByDefault({}) OperatingSystem operatingSystem;
    private @NonNullByDefault({}) List<NetworkIF> networks;
    private @NonNullByDefault({}) List<Display> displays;
    private @NonNullByDefault({}) List<PowerSource> powerSources;
    private @NonNullByDefault({}) List<HWDiskStore> drives;

    // Map containing previous process state to calculate load by process
    private Map<Integer, OSProcess> processTicks = new HashMap<>();

    public static final int PRECISION_AFTER_DECIMAL_SIGN = 1;

    /**
     * Some of the methods used in this constructor execute native code and require execute permissions
     *
     */
    public OSHISystemInfo() {
        logger.debug("OSHISystemInfo service is created");
    }

    @Override
    public void initializeSystemInfo() {
        logger.debug("OSHISystemInfo service starts initializing");

        SystemInfo systemInfo = new SystemInfo();
        hal = systemInfo.getHardware();
        operatingSystem = systemInfo.getOperatingSystem();

        // Doesn't need regular update, they may be queried repeatedly
        cpu = hal.getProcessor();
        drives = hal.getDiskStores();
        memory = hal.getMemory();
        sensors = hal.getSensors();

        computerSystem = hal.getComputerSystem();
        networks = hal.getNetworkIFs();
        displays = hal.getDisplays();
        powerSources = hal.getPowerSources();
    }

    private <T> T getDevice(List<@Nullable T> devices, int index) throws DeviceNotFoundException {
        if (devices.size() <= index) {
            throw new DeviceNotFoundException("Device with index: " + index + " can not be found!");
        }
        return (T) devices.get(index);
    }

    private <T> T getDevice(T @Nullable [] devices, int index) throws DeviceNotFoundException {
        if (devices == null || devices.length <= index) {
            throw new DeviceNotFoundException("Device with index: " + index + " can not be found!");
        }
        return devices[index];
    }

    private OSProcess getProcess(int pid) throws DeviceNotFoundException {
        OSProcess process = operatingSystem.getProcess(pid);
        if (process == null) {
            throw new DeviceNotFoundException("Error while getting information for process with PID " + pid);
        }
        return process;
    }

    @Override
    public StringType getOsFamily() {
        String osFamily = operatingSystem.getFamily();
        return new StringType(osFamily);
    }

    @Override
    public StringType getOsManufacturer() {
        String osManufacturer = operatingSystem.getManufacturer();
        return new StringType(osManufacturer);
    }

    @Override
    public StringType getOsVersion() {
        String osVersion = operatingSystem.getVersionInfo().toString();
        return new StringType(osVersion);
    }

    @Override
    public StringType getDisplayInformation(int index) throws DeviceNotFoundException {
        Display display = getDevice(displays, index);

        byte[] edid = display.getEdid();
        String manufacturer = EdidUtil.getManufacturerID(edid);
        String product = EdidUtil.getProductID(edid);
        String serialNumber = EdidUtil.getSerialNo(edid);
        int width = EdidUtil.getHcm(edid);
        int height = EdidUtil.getVcm(edid);

        String edidFormatString = "Product %s, manufacturer %s, SN: %s, Width: %d, Height: %d";
        String edidInfo = String.format(edidFormatString, product, manufacturer, serialNumber, width, height);
        return new StringType(edidInfo);
    }

    @Override
    public @Nullable DecimalType getSensorsFanSpeed(int index) throws DeviceNotFoundException {
        int[] fanSpeeds = sensors.getFanSpeeds();
        int speed = 0; // 0 means unable to measure speed
        if (index < fanSpeeds.length) {
            speed = fanSpeeds[index];
        } else {
            throw new DeviceNotFoundException();
        }
        return speed > 0 ? new DecimalType(speed) : null;
    }

    @Override
    public @Nullable QuantityType<Time> getBatteryRemainingTime(int index) throws DeviceNotFoundException {
        PowerSource powerSource = getDevice(powerSources, index);
        powerSource.updateAttributes();
        double remainingTimeInSeconds = powerSource.getTimeRemainingEstimated();
        // The getTimeRemaining() method returns (-1.0) if is calculating or (-2.0) if the time is unlimited.
        BigDecimal remainingTime = getTimeInMinutes(remainingTimeInSeconds);
        return remainingTime.signum() == 1 ? new QuantityType<>(remainingTime, Units.MINUTE) : null;
    }

    @Override
    public PercentType getBatteryRemainingCapacity(int index) throws DeviceNotFoundException {
        PowerSource powerSource = getDevice(powerSources, index);
        powerSource.updateAttributes();
        double remainingCapacity = powerSource.getRemainingCapacityPercent();
        BigDecimal remainingCapacityPercents = getPercentsValue(remainingCapacity);
        return new PercentType(remainingCapacityPercents);
    }

    @Override
    public StringType getBatteryName(int index) throws DeviceNotFoundException {
        PowerSource powerSource = getDevice(powerSources, index);
        return new StringType(powerSource.getName());
    }

    @Override
    public CentralProcessor getCPUSpecification() {
        return cpu;
    }

    @Override
    public List<OSFileStore> getFileStorageList() {
        return operatingSystem.getFileSystem().getFileStores();
    }

    @Override
    public List<HWDiskStore> getHardDriveList() {
        return drives;
    }

    @Override
    public GlobalMemory getMemorySpecifications() {
        return memory;
    }

    @Override
    public VirtualMemory getSwapSpecifications() {
        return memory.getVirtualMemory();
    }

    @Override
    public List<NetworkIF> getNetworkInterfaceList() {
        return networks;
    }

    @Override
    public OperatingSystem getOperatingSystem() {
        return operatingSystem;
    }

    @Override
    public Sensors getSensors() {
        return sensors;
    }

    @Override
    public ComputerSystem getSystem() {
        return computerSystem;
    }

    private long getSizeInMB(long sizeInBytes) {
        return Math.round(sizeInBytes / (1024D * 1024));
    }

    private BigDecimal getPercentsValue(double decimalFraction) {
        final BigDecimal result = new BigDecimal(decimalFraction * 100);
        return result.setScale(PRECISION_AFTER_DECIMAL_SIGN, RoundingMode.HALF_UP);
    }

    private BigDecimal getTimeInMinutes(double timeInSeconds) {
        final BigDecimal timeInMinutes = new BigDecimal(timeInSeconds / 60);
        return timeInMinutes.setScale(PRECISION_AFTER_DECIMAL_SIGN, RoundingMode.UP);
    }

    @Override
    public int getCurrentProcessID() {
        return operatingSystem.getProcessId();
    }

    @Override
    public @Nullable StringType getProcessName(int pid) throws DeviceNotFoundException {
        if (pid > 0) {
            OSProcess process = getProcess(pid);
            String name = process.getName();
            return new StringType(name);
        } else {
            return null;
        }
    }

    @Override
    public @Nullable DecimalType getProcessCpuUsage(int pid) throws DeviceNotFoundException {
        if (pid > 0) {
            OSProcess process = getProcess(pid);
            DecimalType load = (processTicks.containsKey(pid))
                    ? new DecimalType(getPercentsValue(process.getProcessCpuLoadBetweenTicks(processTicks.get(pid))))
                    : null;
            processTicks.put(pid, process);
            return load;
        } else {
            return null;
        }
    }

    @Override
    public @Nullable QuantityType<DataAmount> getProcessMemoryUsage(int pid) throws DeviceNotFoundException {
        if (pid > 0) {
            OSProcess process = getProcess(pid);
            long memortInBytes = process.getResidentSetSize();
            long memoryInMB = getSizeInMB(memortInBytes);
            return new QuantityType<>(memoryInMB, Units.MEBIBYTE);
        } else {
            return null;
        }
    }

    @Override
    public @Nullable StringType getProcessPath(int pid) throws DeviceNotFoundException {
        if (pid > 0) {
            OSProcess process = getProcess(pid);
            String path = process.getPath();
            return new StringType(path);
        } else {
            return null;
        }
    }

    @Override
    public @Nullable DecimalType getProcessThreads(int pid) throws DeviceNotFoundException {
        if (pid > 0) {
            OSProcess process = getProcess(pid);
            int threadCount = process.getThreadCount();
            return new DecimalType(threadCount);
        } else {
            return null;
        }
    }

    @Override
    public int getDisplayCount() {
        return displays.size();
    }

    @Override
    public int getPowerSourceCount() {
        return powerSources.size();
    }

    @Override
    public int getFanCount() {
        return sensors.getFanSpeeds().length;
    }
}
