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
import org.openhab.core.library.unit.SIUnits;
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
    private @NonNullByDefault({}) List<OSFileStore> fileStores;
    private @NonNullByDefault({}) List<PowerSource> powerSources;
    private @NonNullByDefault({}) List<HWDiskStore> drives;

    // Array containing cpu tick info to calculate CPU load, according to oshi doc:
    // 8 long values representing time spent in User, Nice, System, Idle, IOwait, IRQ, SoftIRQ, and Steal states
    private long[] ticks = new long[8];
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
        fileStores = operatingSystem.getFileSystem().getFileStores();
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
    public StringType getCpuName() {
        String name = cpu.getProcessorIdentifier().getName();
        return new StringType(name);
    }

    @Override
    public StringType getCpuDescription() {
        String model = cpu.getProcessorIdentifier().getModel();
        String family = cpu.getProcessorIdentifier().getFamily();
        String serialNumber = computerSystem.getSerialNumber();
        String identifier = cpu.getProcessorIdentifier().getIdentifier();
        String vendor = cpu.getProcessorIdentifier().getVendor();
        String architecture = cpu.getProcessorIdentifier().isCpu64bit() ? "64 bit" : "32 bit";
        String descriptionFormatString = "Model: %s %s,family: %s, vendor: %s, sn: %s, identifier: %s ";
        String description = String.format(descriptionFormatString, model, architecture, family, vendor, serialNumber,
                identifier);

        return new StringType(description);
    }

    @Override
    public DecimalType getCpuLogicalCores() {
        int logicalProcessorCount = cpu.getLogicalProcessorCount();
        return new DecimalType(logicalProcessorCount);
    }

    @Override
    public DecimalType getCpuPhysicalCores() {
        int physicalProcessorCount = cpu.getPhysicalProcessorCount();
        return new DecimalType(physicalProcessorCount);
    }

    @Override
    public @Nullable QuantityType<Frequency> getCpuMaxFreq() {
        long maxFreq = cpu.getMaxFreq();
        return maxFreq >= 0 ? new QuantityType<>(maxFreq, Units.HERTZ) : null;
    }

    @Override
    public @Nullable QuantityType<Frequency> getCpuFreq(int logicalProcessorIndex) {
        long freq = cpu.getCurrentFreq()[logicalProcessorIndex];
        return freq >= 0 ? new QuantityType<>(freq, Units.HERTZ) : null;
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
    public @Nullable QuantityType<Temperature> getSensorsCpuTemperature() {
        BigDecimal cpuTemp = new BigDecimal(sensors.getCpuTemperature());
        cpuTemp = cpuTemp.setScale(PRECISION_AFTER_DECIMAL_SIGN, RoundingMode.HALF_UP);
        return cpuTemp.signum() == 1 ? new QuantityType<>(cpuTemp, SIUnits.CELSIUS) : null;
    }

    @Override
    public @Nullable QuantityType<ElectricPotential> getSensorsCpuVoltage() {
        BigDecimal cpuVoltage = new BigDecimal(sensors.getCpuVoltage());
        cpuVoltage = cpuVoltage.setScale(PRECISION_AFTER_DECIMAL_SIGN, RoundingMode.HALF_UP);
        return cpuVoltage.signum() == 1 ? new QuantityType<>(cpuVoltage, Units.VOLT) : null;
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
        String name = powerSource.getName();
        return new StringType(name);
    }

    @Override
    public int getFileStorageCount() {
        return fileStores.size();
    }

    @Override
    public List<OSFileStore> getFileStorageList() {
        return fileStores;
    }

    @Override
    public int getHardDriveCount() {
        return drives.size();
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
    public int getNetworkInterfaceCount() {
        return networks.size();
    }

    @Override
    public List<NetworkIF> getNetworkInterfaceList() {
        return networks;
    }

    private long getSizeInMB(long sizeInBytes) {
        return Math.round(sizeInBytes / (1024D * 1024));
    }

    private BigDecimal getPercentsValue(double decimalFraction) {
        BigDecimal result = new BigDecimal(decimalFraction * 100);
        result = result.setScale(PRECISION_AFTER_DECIMAL_SIGN, RoundingMode.HALF_UP);
        return result;
    }

    private BigDecimal getTimeInMinutes(double timeInSeconds) {
        BigDecimal timeInMinutes = new BigDecimal(timeInSeconds / 60);
        timeInMinutes = timeInMinutes.setScale(PRECISION_AFTER_DECIMAL_SIGN, RoundingMode.UP);
        return timeInMinutes;
    }

    @Override
    public @Nullable PercentType getSystemCpuLoad() {
        PercentType load = (ticks[0] > 0) ? new PercentType(getPercentsValue(cpu.getSystemCpuLoadBetweenTicks(ticks)))
                : null;
        ticks = cpu.getSystemCpuLoadTicks();
        return load;
    }

    /**
     * {@inheritDoc}
     *
     * This information is available only on Mac and Linux OS.
     */
    @Override
    public @Nullable DecimalType getCpuLoad1() {
        BigDecimal avarageCpuLoad = getAvarageCpuLoad(1);
        return avarageCpuLoad.signum() == -1 ? null : new DecimalType(avarageCpuLoad);
    }

    /**
     * {@inheritDoc}
     *
     * This information is available only on Mac and Linux OS.
     */
    @Override
    public @Nullable DecimalType getCpuLoad5() {
        BigDecimal avarageCpuLoad = getAvarageCpuLoad(5);
        return avarageCpuLoad.signum() == -1 ? null : new DecimalType(avarageCpuLoad);
    }

    /**
     * {@inheritDoc}
     *
     * This information is available only on Mac and Linux OS.
     */
    @Override
    public @Nullable DecimalType getCpuLoad15() {
        BigDecimal avarageCpuLoad = getAvarageCpuLoad(15);
        return avarageCpuLoad.signum() == -1 ? null : new DecimalType(avarageCpuLoad);
    }

    private BigDecimal getAvarageCpuLoad(int timeInMinutes) {
        // This parameter is specified in OSHI Javadoc
        int index;
        switch (timeInMinutes) {
            case 1:
                index = 0;
                break;
            case 5:
                index = 1;
                break;
            case 15:
                index = 2;
                break;
            default:
                index = 2;
        }
        double processorLoads[] = cpu.getSystemLoadAverage(index + 1);
        BigDecimal result = new BigDecimal(processorLoads[index]);
        result = result.setScale(PRECISION_AFTER_DECIMAL_SIGN, RoundingMode.HALF_UP);
        return result;
    }

    @Override
    public QuantityType<Time> getCpuUptime() {
        long seconds = operatingSystem.getSystemUptime();
        return new QuantityType<>(getTimeInMinutes(seconds), Units.MINUTE);
    }

    @Override
    public DecimalType getCpuThreads() {
        int threadCount = operatingSystem.getThreadCount();
        return new DecimalType(threadCount);
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
    public int getFileOSStoreCount() {
        return fileStores.size();
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
