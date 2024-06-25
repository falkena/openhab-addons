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
package org.openhab.binding.systeminfo.test.handler;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.mockito.Mockito.when;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.*;

import java.math.BigDecimal;

import javax.measure.quantity.ElectricPotential;
import javax.measure.quantity.Frequency;
import javax.measure.quantity.Temperature;
import javax.measure.quantity.Time;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants;
import org.openhab.binding.systeminfo.internal.handler.SystemInfoComputerHandler;
import org.openhab.binding.systeminfo.internal.model.DeviceNotFoundException;
import org.openhab.core.config.core.Configuration;
import org.openhab.core.library.dimension.DataAmount;
import org.openhab.core.library.types.DecimalType;
import org.openhab.core.library.types.PercentType;
import org.openhab.core.library.types.QuantityType;
import org.openhab.core.library.types.StringType;
import org.openhab.core.library.unit.SIUnits;
import org.openhab.core.library.unit.Units;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.Channel;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusDetail;
import org.openhab.core.thing.ThingStatusInfo;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.BridgeHandler;
import org.openhab.core.thing.binding.builder.BridgeBuilder;
import org.openhab.core.thing.binding.builder.ChannelBuilder;
import org.openhab.core.thing.binding.builder.ThingBuilder;
import org.openhab.core.types.UnDefType;

/**
 * OSGi tests for the {@link SystemInfoComputerHandler}
 *
 * @author Svilen Valkanov - Initial contribution
 * @author Lyubomir Papazov - Created a mock systeminfo object. This way, access to the user's OS will not be required,
 *         but mock data will be used instead, avoiding potential errors from the OS queries.
 * @author Wouter Born - Migrate Groovy to Java tests
 * @author Mark Herwege - Processor frequency channels
 */
@NonNullByDefault
@ExtendWith(MockitoExtension.class)
public class SystemInfoComputerOSGiTest extends SystemInfoOSGiTestBase {
    private static final String TEST_ITEM_NAME = "computer";

    private void initializeThingWithChannelAndPID(String channelID, String acceptedItemType, int pid) {
        final Configuration thingConfig = new Configuration();
        thingConfig.put(HIGH_PRIORITY_REFRESH_TIME, new BigDecimal(DEFAULT_TEST_INTERVAL_HIGH));
        thingConfig.put(MEDIUM_PRIORITY_REFRESH_TIME, new BigDecimal(DEFAULT_TEST_INTERVAL_MEDIUM));

        initializeThing(thingConfig, channelID, acceptedItemType, DEFAULT_CHANNEL_TEST_PRIORITY, pid);

        final Bridge bridge = systemInfoBridge;
        if (bridge == null) {
            throw new AssertionError("Bridge is null");
        }

        final ChannelUID channelUID = new ChannelUID(bridge.getUID(), channelID);
        initializeItem(channelUID, TEST_ITEM_NAME, acceptedItemType);
    }

    private void initializeThingWithChannelAndPriority(String channelID, String acceptedItemType, String priority) {
        final Configuration thingConfig = new Configuration();
        thingConfig.put(HIGH_PRIORITY_REFRESH_TIME, new BigDecimal(DEFAULT_TEST_INTERVAL_HIGH));
        thingConfig.put(MEDIUM_PRIORITY_REFRESH_TIME, new BigDecimal(DEFAULT_TEST_INTERVAL_MEDIUM));

        initializeThing(thingConfig, channelID, acceptedItemType, priority, DEFAULT_CHANNEL_PID);

        final Bridge bridge = systemInfoBridge;
        if (bridge == null) {
            throw new AssertionError("Bridge is null");
        }

        final ChannelUID channelUID = new ChannelUID(bridge.getUID(), channelID);
        initializeItem(channelUID, TEST_ITEM_NAME, acceptedItemType);
    }

    @Override
    protected void initializeThingWithChannel(String channelID, String acceptedItemType) {
        super.initializeThingWithChannel(channelID, acceptedItemType);

        final Bridge bridge = systemInfoBridge;
        if (bridge == null) {
            throw new AssertionError("Bridge is null");
        }

        final ChannelUID channelUID = new ChannelUID(bridge.getUID(), channelID);
        initializeItem(channelUID, TEST_ITEM_NAME, acceptedItemType);
    }

    @Test
    public void assertInvalidThingConfigurationValuesAreHandled() {
        final Configuration configuration = new Configuration();
        configuration.put(HIGH_PRIORITY_REFRESH_TIME, new BigDecimal(-5)); // invalid value - must be positive
        configuration.put(MEDIUM_PRIORITY_REFRESH_TIME, new BigDecimal(3));

        final ThingUID thingUID = new ThingUID(BRIDGE_TYPE_COMPUTER, DEFAULT_TEST_THING_NAME);

        final BridgeBuilder builder = BridgeBuilder.create(BRIDGE_TYPE_COMPUTER, thingUID);
        builder.withConfiguration(configuration);

        final Bridge bridge = builder.build();
        assertThat(bridge, is(notNullValue()));
        managedThingProvider.add(bridge);

        final BridgeHandler handler = bridge.getHandler();
        if (handler == null) {
            throw new AssertionError("Bridge handler is null");
        }
        assertThat(handler, is(instanceOf(SystemInfoComputerHandler.class)));
        handler.initialize();

        waitForAssert(() -> {
            assertThat("Invalid configuration is used", bridge.getStatus(), is(equalTo(ThingStatus.OFFLINE)));
            final ThingStatusInfo statusInfo = bridge.getStatusInfo();
            assertThat(statusInfo.getStatusDetail(), is(equalTo(ThingStatusDetail.HANDLER_INITIALIZING_ERROR)));
            assertThat(statusInfo.getDescription(), is(equalTo("@text/offline.cannot-initialize")));
        });

        systemInfoBridge = bridge;
    }

    @Test
    public void assertMediumPriorityChannelIsUpdated() {
        String channnelID = DEFAULT_TEST_CHANNEL_ID;
        String priority = "Medium";

        initializeThingWithChannelAndPriority(channnelID, "Number", priority);
        assertItemState(TEST_ITEM_NAME, priority, UnDefType.UNDEF);
    }

    @Test
    public void assertStateOfSecondDeviceIsUpdated() {
        // This test assumes that at least 2 network interfaces are present on the test platform
        int deviceIndex = 1;
        String channnelID = "network" + deviceIndex + "#mac";

        initializeThingWithChannel(channnelID, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, UnDefType.UNDEF);
    }

    @Test
    public void assertChannelCpuMaxFreq() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_CPU_MAXFREQ;

        QuantityType<Frequency> mockedCpuMaxFreqValue = new QuantityType<>(2500, Units.HERTZ);
        when(mockedSystemInfo.getCpuMaxFreq()).thenReturn(mockedCpuMaxFreqValue);

        initializeThingWithChannel(channnelID, "Number:Frequency");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedCpuMaxFreqValue);
    }

    @Test
    public void assertChannelCpuFreq() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_CPU_FREQ;

        QuantityType<Frequency> mockedCpuFreqValue = new QuantityType<>(2500, Units.HERTZ);
        when(mockedSystemInfo.getCpuFreq(0)).thenReturn(mockedCpuFreqValue);

        initializeThingWithChannel(channnelID, "Number:Frequency");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedCpuFreqValue);
    }

    @Test
    public void assertChannelCpuLoadIsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_CPU_LOAD;

        PercentType mockedCpuLoadValue = new PercentType(9);
        when(mockedSystemInfo.getSystemCpuLoad()).thenReturn(mockedCpuLoadValue);

        initializeThingWithChannel(channnelID, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedCpuLoadValue);
    }

    @Test
    public void assertChannelCpuLoad1IsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_CPU_LOAD_1;

        DecimalType mockedCpuLoad1Value = new DecimalType(1.1);
        when(mockedSystemInfo.getCpuLoad1()).thenReturn(mockedCpuLoad1Value);

        initializeThingWithChannel(channnelID, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedCpuLoad1Value);
    }

    @Test
    public void assertChannelCpuLoad5IsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_CPU_LOAD_5;

        DecimalType mockedCpuLoad5Value = new DecimalType(5.5);
        when(mockedSystemInfo.getCpuLoad5()).thenReturn(mockedCpuLoad5Value);

        initializeThingWithChannel(channnelID, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedCpuLoad5Value);
    }

    @Test
    public void assertChannelCpuLoad15IsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_CPU_LOAD_15;

        DecimalType mockedCpuLoad15Value = new DecimalType(15.15);
        when(mockedSystemInfo.getCpuLoad15()).thenReturn(mockedCpuLoad15Value);

        initializeThingWithChannel(channnelID, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedCpuLoad15Value);
    }

    @Test
    public void assertChannelCpuThreadsIsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_CPU_THREADS;

        DecimalType mockedCpuThreadsValue = new DecimalType(16);
        when(mockedSystemInfo.getCpuThreads()).thenReturn(mockedCpuThreadsValue);

        initializeThingWithChannel(channnelID, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedCpuThreadsValue);
    }

    @Test
    public void assertChannelCpuUptimeIsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_CPU_UPTIME;

        QuantityType<Time> mockedCpuUptimeValue = new QuantityType<>(100, Units.MINUTE);
        when(mockedSystemInfo.getCpuUptime()).thenReturn(mockedCpuUptimeValue);

        initializeThingWithChannel(channnelID, "Number:Time");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedCpuUptimeValue);
    }

    @Test
    public void assertChannelCpuDescriptionIsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_CPU_DESCRIPTION;

        StringType mockedCpuDescriptionValue = new StringType("Mocked Cpu Descr");
        when(mockedSystemInfo.getCpuDescription()).thenReturn(mockedCpuDescriptionValue);

        initializeThingWithChannel(channnelID, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedCpuDescriptionValue);
    }

    @Test
    public void assertChannelCpuNameIsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_CPU_NAME;

        StringType mockedCpuNameValue = new StringType("Mocked Cpu Name");
        when(mockedSystemInfo.getCpuName()).thenReturn(mockedCpuNameValue);

        initializeThingWithChannel(channnelID, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedCpuNameValue);
    }

    @Test
    public void assertChannelMemoryAvailableIsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_MEMORY_AVAILABLE;

        QuantityType<DataAmount> mockedMemoryAvailableValue = new QuantityType<>(1000, Units.MEBIBYTE);
        when(mockedSystemInfo.getMemoryAvailable()).thenReturn(mockedMemoryAvailableValue);

        initializeThingWithChannel(channnelID, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedMemoryAvailableValue);
    }

    @Test
    public void assertChannelMemoryUsedIsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_MEMORY_USED;

        QuantityType<DataAmount> mockedMemoryUsedValue = new QuantityType<>(24, Units.MEBIBYTE);
        when(mockedSystemInfo.getMemoryUsed()).thenReturn(mockedMemoryUsedValue);

        initializeThingWithChannel(channnelID, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedMemoryUsedValue);
    }

    @Test
    public void assertChannelMemoryTotalIsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_MEMORY_TOTAL;

        QuantityType<DataAmount> mockedMemoryTotalValue = new QuantityType<>(1024, Units.MEBIBYTE);
        when(mockedSystemInfo.getMemoryTotal()).thenReturn(mockedMemoryTotalValue);

        initializeThingWithChannel(channnelID, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedMemoryTotalValue);
    }

    @Test
    public void assertChannelMemoryAvailablePercentIsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_MEMORY_AVAILABLE_PERCENT;

        PercentType mockedMemoryAvailablePercentValue = new PercentType(97);
        when(mockedSystemInfo.getMemoryAvailablePercent()).thenReturn(mockedMemoryAvailablePercentValue);

        initializeThingWithChannel(channnelID, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedMemoryAvailablePercentValue);
    }

    @Test
    public void assertChannelSwapAvailableIsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_SWAP_AVAILABLE;

        QuantityType<DataAmount> mockedSwapAvailableValue = new QuantityType<>(482, Units.MEBIBYTE);
        when(mockedSystemInfo.getSwapAvailable()).thenReturn(mockedSwapAvailableValue);

        initializeThingWithChannel(channnelID, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedSwapAvailableValue);
    }

    @Test
    public void assertChannelSwapUsedIsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_SWAP_USED;

        QuantityType<DataAmount> mockedSwapUsedValue = new QuantityType<>(30, Units.MEBIBYTE);
        when(mockedSystemInfo.getSwapUsed()).thenReturn(mockedSwapUsedValue);

        initializeThingWithChannel(channnelID, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedSwapUsedValue);
    }

    @Test
    public void assertChannelSwapTotalIsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_SWAP_TOTAL;

        QuantityType<DataAmount> mockedSwapTotalValue = new QuantityType<>(512, Units.MEBIBYTE);
        when(mockedSystemInfo.getSwapTotal()).thenReturn(mockedSwapTotalValue);

        initializeThingWithChannel(channnelID, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedSwapTotalValue);
    }

    @Test
    public void assertChannelSwapAvailablePercentIsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_SWAP_AVAILABLE_PERCENT;

        PercentType mockedSwapAvailablePercentValue = new PercentType(94);
        when(mockedSystemInfo.getSwapAvailablePercent()).thenReturn(mockedSwapAvailablePercentValue);

        initializeThingWithChannel(channnelID, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedSwapAvailablePercentValue);
    }

    @Test
    public void assertChannelStorageNameIsUpdated() throws DeviceNotFoundException {
        String channnelID = SystemInfoBindingConstants.CHANNEL_STORAGE_NAME;

        StringType mockedStorageName = new StringType("Mocked Storage Name");
        when(mockedSystemInfo.getStorageName(DEFAULT_DEVICE_INDEX)).thenReturn(mockedStorageName);

        initializeThingWithChannel(channnelID, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedStorageName);
    }

    @Test
    public void assertChannelStorageTypeIsUpdated() throws DeviceNotFoundException {
        String channnelID = SystemInfoBindingConstants.CHANNEL_STORAGE_TYPE;

        StringType mockedStorageType = new StringType("Mocked Storage Type");
        when(mockedSystemInfo.getStorageType(DEFAULT_DEVICE_INDEX)).thenReturn(mockedStorageType);

        initializeThingWithChannel(channnelID, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedStorageType);
    }

    @Test
    public void assertChannelStorageDescriptionIsUpdated() throws DeviceNotFoundException {
        String channnelID = SystemInfoBindingConstants.CHANNEL_STORAGE_DESCRIPTION;

        StringType mockedStorageDescription = new StringType("Mocked Storage Description");
        when(mockedSystemInfo.getStorageDescription(DEFAULT_DEVICE_INDEX)).thenReturn(mockedStorageDescription);

        initializeThingWithChannel(channnelID, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedStorageDescription);
    }

    @Test
    public void assertChannelStorageAvailableIsUpdated() throws DeviceNotFoundException {
        String channnelID = SystemInfoBindingConstants.CHANNEL_STORAGE_AVAILABLE;

        QuantityType<DataAmount> mockedStorageAvailableValue = new QuantityType<>(2000, Units.MEBIBYTE);
        when(mockedSystemInfo.getStorageAvailable(DEFAULT_DEVICE_INDEX)).thenReturn(mockedStorageAvailableValue);

        initializeThingWithChannel(channnelID, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedStorageAvailableValue);
    }

    @Test
    public void assertChannelStorageUsedIsUpdated() throws DeviceNotFoundException {
        String channnelID = SystemInfoBindingConstants.CHANNEL_STORAGE_USED;

        QuantityType<DataAmount> mockedStorageUsedValue = new QuantityType<>(500, Units.MEBIBYTE);
        when(mockedSystemInfo.getStorageUsed(DEFAULT_DEVICE_INDEX)).thenReturn(mockedStorageUsedValue);

        initializeThingWithChannel(channnelID, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedStorageUsedValue);
    }

    @Test
    public void assertChannelStorageTotalIsUpdated() throws DeviceNotFoundException {
        String channnelID = SystemInfoBindingConstants.CHANNEL_STORAGE_TOTAL;

        QuantityType<DataAmount> mockedStorageTotalValue = new QuantityType<>(2500, Units.MEBIBYTE);
        when(mockedSystemInfo.getStorageTotal(DEFAULT_DEVICE_INDEX)).thenReturn(mockedStorageTotalValue);

        initializeThingWithChannel(channnelID, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedStorageTotalValue);
    }

    @Test
    public void assertChannelStorageAvailablePercentIsUpdated() throws DeviceNotFoundException {
        String channnelID = SystemInfoBindingConstants.CHANNEL_STORAGE_AVAILABLE_PERCENT;

        PercentType mockedStorageAvailablePercent = new PercentType(20);
        when(mockedSystemInfo.getStorageAvailablePercent(DEFAULT_DEVICE_INDEX))
                .thenReturn(mockedStorageAvailablePercent);

        initializeThingWithChannel(channnelID, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedStorageAvailablePercent);
    }

    // Re-enable this previously disabled test, as it is not relying on hardware anymore, but a mocked object
    // There is a bug opened for this issue - https://github.com/dblock/oshi/issues/185
    @Test
    public void assertChannelSensorsCpuTempIsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_SENSORS_CPU_TEMPERATURE;

        QuantityType<Temperature> mockedSensorsCpuTemperatureValue = new QuantityType<>(60, SIUnits.CELSIUS);
        when(mockedSystemInfo.getSensorsCpuTemperature()).thenReturn(mockedSensorsCpuTemperatureValue);

        initializeThingWithChannel(channnelID, "Number:Temperature");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedSensorsCpuTemperatureValue);
    }

    @Test
    public void assertChannelSensorsCpuVoltageIsUpdated() {
        String channnelID = SystemInfoBindingConstants.CHANNEL_SENOSRS_CPU_VOLTAGE;

        QuantityType<ElectricPotential> mockedSensorsCpuVoltageValue = new QuantityType<>(1000, Units.VOLT);
        when(mockedSystemInfo.getSensorsCpuVoltage()).thenReturn(mockedSensorsCpuVoltageValue);

        initializeThingWithChannel(channnelID, "Number:ElectricPotential");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedSensorsCpuVoltageValue);
    }

    @Test
    public void assertChannelSensorsFanSpeedIsUpdated() throws DeviceNotFoundException {
        String channnelID = SystemInfoBindingConstants.CHANNEL_SENSORS_FAN_SPEED;

        DecimalType mockedSensorsCpuFanSpeedValue = new DecimalType(180);
        when(mockedSystemInfo.getSensorsFanSpeed(DEFAULT_DEVICE_INDEX)).thenReturn(mockedSensorsCpuFanSpeedValue);

        initializeThingWithChannel(channnelID, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedSensorsCpuFanSpeedValue);
    }

    @Test
    public void assertChannelBatteryNameIsUpdated() throws DeviceNotFoundException {
        String channnelID = SystemInfoBindingConstants.CHANNEL_BATTERY_NAME;

        StringType mockedBatteryName = new StringType("Mocked Battery Name");
        when(mockedSystemInfo.getBatteryName(DEFAULT_DEVICE_INDEX)).thenReturn(mockedBatteryName);

        initializeThingWithChannel(channnelID, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedBatteryName);
    }

    @Test
    public void assertChannelBatteryRemainingCapacityIsUpdated() throws DeviceNotFoundException {
        String channnelID = SystemInfoBindingConstants.CHANNEL_BATTERY_REMAINING_CAPACITY;

        PercentType mockedBatteryRemainingCapacity = new PercentType(20);
        when(mockedSystemInfo.getBatteryRemainingCapacity(DEFAULT_DEVICE_INDEX))
                .thenReturn(mockedBatteryRemainingCapacity);

        initializeThingWithChannel(channnelID, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedBatteryRemainingCapacity);
    }

    @Test
    public void assertChannelBatteryRemainingTimeIsUpdated() throws DeviceNotFoundException {
        String channnelID = SystemInfoBindingConstants.CHANNEL_BATTERY_REMAINING_TIME;

        QuantityType<Time> mockedBatteryRemainingTime = new QuantityType<>(3600, Units.MINUTE);
        when(mockedSystemInfo.getBatteryRemainingTime(DEFAULT_DEVICE_INDEX)).thenReturn(mockedBatteryRemainingTime);

        initializeThingWithChannel(channnelID, "Number:Time");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedBatteryRemainingTime);
    }

    @Test
    public void assertChannelDisplayInformationIsUpdated() throws DeviceNotFoundException {
        String channnelID = SystemInfoBindingConstants.CHANNEL_DISPLAY_INFORMATION;

        StringType mockedDisplayInfo = new StringType("Mocked Display Information");
        when(mockedSystemInfo.getDisplayInformation(DEFAULT_DEVICE_INDEX)).thenReturn(mockedDisplayInfo);

        initializeThingWithChannel(channnelID, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedDisplayInfo);
    }

    @Test
    public void assertChannelProcessThreadsIsUpdatedWithPIDse() throws DeviceNotFoundException {
        String channnelID = SystemInfoBindingConstants.CHANNEL_PROCESS_THREADS;

        // The pid of the System idle process in Windows
        int pid = 0;

        DecimalType mockedProcessThreadsCount = new DecimalType(4);
        when(mockedSystemInfo.getProcessThreads(pid)).thenReturn(mockedProcessThreadsCount);

        initializeThingWithChannelAndPID(channnelID, "Number", pid);
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedProcessThreadsCount);
    }

    @Test
    public void assertChannelProcessPathIsUpdatedWithPIDset() throws DeviceNotFoundException {
        String channnelID = SystemInfoBindingConstants.CHANNEL_PROCESS_PATH;

        // The pid of the System idle process in Windows
        int pid = 0;

        StringType mockedProcessPath = new StringType("C:\\Users\\MockedUser\\Process");
        when(mockedSystemInfo.getProcessPath(pid)).thenReturn(mockedProcessPath);

        initializeThingWithChannelAndPID(channnelID, "String", pid);
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedProcessPath);
    }

    @Test
    public void assertChannelProcessNameIsUpdatedWithPIDset() throws DeviceNotFoundException {
        String channnelID = SystemInfoBindingConstants.CHANNEL_PROCESS_NAME;

        // The pid of the System idle process in Windows
        int pid = 0;

        StringType mockedProcessName = new StringType("MockedProcess.exe");
        when(mockedSystemInfo.getProcessName(pid)).thenReturn(mockedProcessName);

        initializeThingWithChannelAndPID(channnelID, "String", pid);
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedProcessName);
    }

    @Test
    public void assertChannelProcessMemoryIsUpdatedWithPIDset() throws DeviceNotFoundException {
        String channnelID = SystemInfoBindingConstants.CHANNEL_PROCESS_MEMORY;

        // The pid of the System idle process in Windows
        int pid = 0;

        QuantityType<DataAmount> mockedProcessMemory = new QuantityType<>(450, Units.MEBIBYTE);
        when(mockedSystemInfo.getProcessMemoryUsage(pid)).thenReturn(mockedProcessMemory);

        initializeThingWithChannelAndPID(channnelID, "Number:DataAmount", pid);
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedProcessMemory);
    }

    @Test
    public void assertChannelProcessLoadIsUpdatedWithPIDset() throws DeviceNotFoundException {
        String channnelID = SystemInfoBindingConstants.CHANNEL_PROCESS_LOAD;

        // The pid of the System idle process in Windows
        int pid = 0;

        DecimalType mockedProcessLoad = new DecimalType(3);
        when(mockedSystemInfo.getProcessCpuUsage(pid)).thenReturn(mockedProcessLoad);

        initializeThingWithChannelAndPID(channnelID, "Number", pid);
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedProcessLoad);
    }

    @Test
    public void testThingHandlesChannelPriorityChange() {
        String priorityKey = "priority";
        String pidKey = "pid";
        String newPriority = "Low";

        String acceptedItemType = "Number";
        initializeThingWithChannel(DEFAULT_TEST_CHANNEL_ID, acceptedItemType);

        final Bridge bridge = systemInfoBridge;
        if (bridge == null) {
            throw new AssertionError("Bridge is null");
        }

        final Channel channel = bridge.getChannel(DEFAULT_TEST_CHANNEL_ID);
        if (channel == null) {
            throw new AssertionError("Channel is null");
        }

        final BridgeHandler handler = bridge.getHandler();
        if (handler == null) {
            throw new AssertionError("Bridge handler is null");
        }
        assertThat(handler, is(instanceOf(SystemInfoComputerHandler.class)));

        final SystemInfoComputerHandler computerHandler = (SystemInfoComputerHandler) handler;
        waitForAssert(() -> {
            assertThat("The initial priority of channel " + channel.getUID() + " is not as expected.",
                    channel.getConfiguration().get(priorityKey), is(equalTo(DEFAULT_CHANNEL_TEST_PRIORITY)));
            assertThat(computerHandler.getHighPriorityChannels().contains(channel.getUID()), is(true));
        });

        // Change the priority of a channel, keep the pid
        final Configuration updatedConfig = new Configuration();
        updatedConfig.put(priorityKey, newPriority);
        updatedConfig.put(pidKey, channel.getConfiguration().get(pidKey));
        Channel updatedChannel = ChannelBuilder.create(channel.getUID(), channel.getAcceptedItemType())
                .withType(channel.getChannelTypeUID()).withKind(channel.getKind()).withConfiguration(updatedConfig)
                .build();

        Thing updatedThing = ThingBuilder.create(bridge.getThingTypeUID(), bridge.getUID())
                .withConfiguration(bridge.getConfiguration()).withChannel(updatedChannel).build();

        handler.thingUpdated(updatedThing);

        waitForAssert(() -> {
            assertThat("The priority of the channel was not updated: ", channel.getConfiguration().get(priorityKey),
                    is(equalTo(newPriority)));
            assertThat(computerHandler.getLowPriorityChannels().contains(channel.getUID()), is(true));
        });
    }
}
