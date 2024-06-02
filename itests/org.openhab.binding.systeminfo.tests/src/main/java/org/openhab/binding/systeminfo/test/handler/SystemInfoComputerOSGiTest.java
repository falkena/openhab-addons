/**
 * Copyright (c) 2010-2024 Contributors to the openHAB project
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
import java.util.Map;

import javax.measure.quantity.ElectricPotential;
import javax.measure.quantity.Frequency;
import javax.measure.quantity.Temperature;
import javax.measure.quantity.Time;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
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
@MockitoSettings(strictness = Strictness.LENIENT)
public class SystemInfoComputerOSGiTest extends SystemInfoOSGiTestBase {
    private static final String TEST_ITEM_NAME = "computer";

    private void initializeThingWithChannelAndPID(String channelID, String acceptedItemType, int pid) {
        Configuration thingConfig = new Configuration();
        thingConfig.put(HIGH_PRIORITY_REFRESH_TIME, new BigDecimal(DEFAULT_TEST_INTERVAL_HIGH));
        thingConfig.put(MEDIUM_PRIORITY_REFRESH_TIME, new BigDecimal(DEFAULT_TEST_INTERVAL_MEDIUM));

        initializeThing(thingConfig, channelID, acceptedItemType, DEFAULT_CHANNEL_TEST_PRIORITY, pid);

        final Bridge bridge = systemInfoBridge;
        assertThat(bridge, is(notNullValue()));

        final ChannelUID channelUID = new ChannelUID(bridge.getUID(), channelID);
        initializeItem(channelUID, TEST_ITEM_NAME, acceptedItemType);
    }

    private void initializeThingWithChannelAndPriority(String channelID, String acceptedItemType, String priority) {
        Configuration thingConfig = new Configuration();
        thingConfig.put(HIGH_PRIORITY_REFRESH_TIME, new BigDecimal(DEFAULT_TEST_INTERVAL_HIGH));
        thingConfig.put(MEDIUM_PRIORITY_REFRESH_TIME, new BigDecimal(DEFAULT_TEST_INTERVAL_MEDIUM));

        initializeThing(thingConfig, channelID, acceptedItemType, priority, DEFAULT_CHANNEL_PID);

        final Bridge bridge = systemInfoBridge;
        assertThat(bridge, is(notNullValue()));

        final ChannelUID channelUID = new ChannelUID(bridge.getUID(), channelID);
        initializeItem(channelUID, TEST_ITEM_NAME, acceptedItemType);
    }

    @Override
    protected void initializeThingWithChannel(String channelID, String acceptedItemType) {
        super.initializeThingWithChannel(channelID, acceptedItemType);

        final Bridge bridge = systemInfoBridge;
        assertThat(bridge, is(notNullValue()));

        final ChannelUID channelUID = new ChannelUID(bridge.getUID(), channelID);
        initializeItem(channelUID, TEST_ITEM_NAME, acceptedItemType);
    }

    @Test
    public void assertInvalidThingConfigurationValuesAreHandled() {
        Configuration configuration = new Configuration();
        configuration.put(HIGH_PRIORITY_REFRESH_TIME, new BigDecimal(-5)); // invalid value - must be positive
        configuration.put(MEDIUM_PRIORITY_REFRESH_TIME, new BigDecimal(3));

        final ThingUID thingUID = new ThingUID(BRIDGE_TYPE_COMPUTER, DEFAULT_TEST_THING_NAME);

        final BridgeBuilder builder = BridgeBuilder.create(BRIDGE_TYPE_COMPUTER, thingUID);
        builder.withConfiguration(configuration);
        // Make sure the thingTypeVersion matches the highest version in the update instructions of
        // the binding to avoid new channels being added and the thing not initializing
        builder.withProperties(Map.of("thingTypeVersion", "1"));

        final Bridge bridge = builder.build();
        assertThat(bridge, is(notNullValue()));
        managedThingProvider.add(bridge);
        systemInfoBridge = bridge;

        final BridgeHandler handler = bridge.getHandler();
        assertThat(handler, is(notNullValue()));
        assertThat(handler, is(instanceOf(SystemInfoComputerHandler.class)));
        handler.initialize();

        waitForAssert(() -> {
            assertThat("Invalid configuration is used", bridge.getStatus(), is(equalTo(ThingStatus.OFFLINE)));
            final ThingStatusInfo statusInfo = bridge.getStatusInfo();
            assertThat(statusInfo.getStatusDetail(), is(equalTo(ThingStatusDetail.HANDLER_INITIALIZING_ERROR)));
            assertThat(statusInfo.getDescription(), is(equalTo("@text/offline.cannot-initialize")));
        });
    }

    @Test
    public void assertMediumPriorityChannelIsUpdated() {
        final String priority = "Medium";

        initializeThingWithChannelAndPriority(DEFAULT_TEST_CHANNEL_ID, "Number", priority);
        assertItemState(TEST_ITEM_NAME, priority, UnDefType.UNDEF);
    }

    @Test
    public void assertStateOfSecondDeviceIsUpdated() {
        // This test assumes that at least 2 network interfaces are present on the test platform
        initializeThingWithChannel(String.format("network%d#mac", 1), "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, UnDefType.UNDEF);
    }

    @Test
    public void assertChannelCpuMaxFreq() {
        final QuantityType<Frequency> mockedValue = new QuantityType<>(2500, Units.HERTZ);
        when(mockedSystemInfo.getCpuMaxFreq()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_CPU_MAXFREQ, "Number:Frequency");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelCpuFreq() {
        final QuantityType<Frequency> mockedValue = new QuantityType<>(2500, Units.HERTZ);
        when(mockedSystemInfo.getCpuFreq(0)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_CPU_FREQ, "Number:Frequency");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelCpuLoadIsUpdated() {
        final PercentType mockedValue = new PercentType(9);
        when(mockedSystemInfo.getSystemCpuLoad()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_CPU_LOAD, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelCpuLoad1IsUpdated() {
        final DecimalType mockedValue = new DecimalType(1.1);
        when(mockedSystemInfo.getCpuLoad1()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_CPU_LOAD_1, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelCpuLoad5IsUpdated() {
        final DecimalType mockedValue = new DecimalType(5.5);
        when(mockedSystemInfo.getCpuLoad5()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_CPU_LOAD_5, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelCpuLoad15IsUpdated() {
        final DecimalType mockedValue = new DecimalType(15.15);
        when(mockedSystemInfo.getCpuLoad15()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_CPU_LOAD_15, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelCpuThreadsIsUpdated() {
        final DecimalType mockedValue = new DecimalType(16);
        when(mockedSystemInfo.getCpuThreads()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_CPU_THREADS, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelCpuUptimeIsUpdated() {
        final QuantityType<Time> mockedValue = new QuantityType<>(100, Units.MINUTE);
        when(mockedSystemInfo.getCpuUptime()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_CPU_UPTIME, "Number:Time");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelCpuDescriptionIsUpdated() {
        final StringType mockedValue = new StringType("Mocked Cpu Descr");
        when(mockedSystemInfo.getCpuDescription()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_CPU_DESCRIPTION, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelCpuNameIsUpdated() {
        final StringType mockedValue = new StringType("Mocked Cpu Name");
        when(mockedSystemInfo.getCpuName()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_CPU_NAME, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelMemoryAvailableIsUpdated() {
        final QuantityType<DataAmount> mockedValue = new QuantityType<>(1000, Units.MEBIBYTE);
        when(mockedSystemInfo.getMemoryAvailable()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_MEMORY_AVAILABLE, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelMemoryUsedIsUpdated() {
        final QuantityType<DataAmount> mockedValue = new QuantityType<>(24, Units.MEBIBYTE);
        when(mockedSystemInfo.getMemoryUsed()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_MEMORY_USED, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelMemoryTotalIsUpdated() {
        final QuantityType<DataAmount> mockedValue = new QuantityType<>(1024, Units.MEBIBYTE);
        when(mockedSystemInfo.getMemoryTotal()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_MEMORY_TOTAL, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelMemoryAvailablePercentIsUpdated() {
        final PercentType mockedValue = new PercentType(97);
        when(mockedSystemInfo.getMemoryAvailablePercent()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_MEMORY_AVAILABLE_PERCENT, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSwapAvailableIsUpdated() {
        final QuantityType<DataAmount> mockedValue = new QuantityType<>(482, Units.MEBIBYTE);
        when(mockedSystemInfo.getSwapAvailable()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_SWAP_AVAILABLE, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSwapUsedIsUpdated() {
        final QuantityType<DataAmount> mockedValue = new QuantityType<>(30, Units.MEBIBYTE);
        when(mockedSystemInfo.getSwapUsed()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_SWAP_USED, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSwapTotalIsUpdated() {
        final QuantityType<DataAmount> mockedValue = new QuantityType<>(512, Units.MEBIBYTE);
        when(mockedSystemInfo.getSwapTotal()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_SWAP_TOTAL, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSwapAvailablePercentIsUpdated() {
        final PercentType mockedValue = new PercentType(94);
        when(mockedSystemInfo.getSwapAvailablePercent()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_SWAP_AVAILABLE_PERCENT, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelStorageNameIsUpdated() throws DeviceNotFoundException {
        final StringType mockedValue = new StringType("Mocked Storage Name");
        when(mockedSystemInfo.getStorageName(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_STORAGE_NAME, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelStorageTypeIsUpdated() throws DeviceNotFoundException {
        final StringType mockedValue = new StringType("Mocked Storage Type");
        when(mockedSystemInfo.getStorageType(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_STORAGE_TYPE, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelStorageDescriptionIsUpdated() throws DeviceNotFoundException {
        final StringType mockedValue = new StringType("Mocked Storage Description");
        when(mockedSystemInfo.getStorageDescription(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_STORAGE_DESCRIPTION, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelStorageAvailableIsUpdated() throws DeviceNotFoundException {
        final QuantityType<DataAmount> mockedValue = new QuantityType<>(2000, Units.MEBIBYTE);
        when(mockedSystemInfo.getStorageAvailable(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_STORAGE_AVAILABLE, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelStorageUsedIsUpdated() throws DeviceNotFoundException {
        final QuantityType<DataAmount> mockedValue = new QuantityType<>(500, Units.MEBIBYTE);
        when(mockedSystemInfo.getStorageUsed(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_STORAGE_USED, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelStorageTotalIsUpdated() throws DeviceNotFoundException {
        final QuantityType<DataAmount> mockedValue = new QuantityType<>(2500, Units.MEBIBYTE);
        when(mockedSystemInfo.getStorageTotal(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_STORAGE_TOTAL, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelStorageAvailablePercentIsUpdated() throws DeviceNotFoundException {
        final PercentType mockedValue = new PercentType(20);
        when(mockedSystemInfo.getStorageAvailablePercent(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_STORAGE_AVAILABLE_PERCENT, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    // Re-enable this previously disabled test, as it is not relying on hardware anymore, but a mocked object
    // There is a bug opened for this issue - https://github.com/dblock/oshi/issues/185
    @Test
    public void assertChannelSensorsCpuTempIsUpdated() {
        final QuantityType<Temperature> mockedValue = new QuantityType<>(60, SIUnits.CELSIUS);
        when(mockedSystemInfo.getSensorsCpuTemperature()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_SENSORS_CPU_TEMPERATURE, "Number:Temperature");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSensorsCpuVoltageIsUpdated() {
        final QuantityType<ElectricPotential> mockedValue = new QuantityType<>(1000, Units.VOLT);
        when(mockedSystemInfo.getSensorsCpuVoltage()).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_SENOSRS_CPU_VOLTAGE, "Number:ElectricPotential");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSensorsFanSpeedIsUpdated() throws DeviceNotFoundException {
        final DecimalType mockedValue = new DecimalType(180);
        when(mockedSystemInfo.getSensorsFanSpeed(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_SENSORS_FAN_SPEED, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelBatteryNameIsUpdated() throws DeviceNotFoundException {
        final StringType mockedValue = new StringType("Mocked Battery Name");
        when(mockedSystemInfo.getBatteryName(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_BATTERY_NAME, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelBatteryRemainingCapacityIsUpdated() throws DeviceNotFoundException {
        final PercentType mockedValue = new PercentType(20);
        when(mockedSystemInfo.getBatteryRemainingCapacity(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_BATTERY_REMAINING_CAPACITY, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelBatteryRemainingTimeIsUpdated() throws DeviceNotFoundException {
        final QuantityType<Time> mockedValue = new QuantityType<>(3600, Units.MINUTE);
        when(mockedSystemInfo.getBatteryRemainingTime(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_BATTERY_REMAINING_TIME, "Number:Time");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelDisplayInformationIsUpdated() throws DeviceNotFoundException {
        final StringType mockedValue = new StringType("Mocked Display Information");
        when(mockedSystemInfo.getDisplayInformation(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_DISPLAY_INFORMATION, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelNetworkIpIsUpdated() throws DeviceNotFoundException {
        final StringType mockedValue = new StringType("192.168.1.0");
        when(mockedSystemInfo.getNetworkIp(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_NETWORK_IP, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelNetworkMacIsUpdated() throws DeviceNotFoundException {
        final StringType mockedValue = new StringType("AB-10-11-12-13-14");
        when(mockedSystemInfo.getNetworkMac(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_NETWORK_MAC, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelNetworkDataSentIsUpdated() throws DeviceNotFoundException {
        final QuantityType<DataAmount> mockedValue = new QuantityType<>(1000, Units.MEBIBYTE);
        when(mockedSystemInfo.getNetworkDataSent(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_NETWORK_DATA_SENT, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelNetworkDataReceivedIsUpdated() throws DeviceNotFoundException {
        final QuantityType<DataAmount> mockedValue = new QuantityType<>(800, Units.MEBIBYTE);
        when(mockedSystemInfo.getNetworkDataReceived(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_NETWORK_DATA_RECEIVED, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelNetworkPacketsSentIsUpdated() throws DeviceNotFoundException {
        final DecimalType mockedValue = new DecimalType(50);
        when(mockedSystemInfo.getNetworkPacketsSent(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_NETWORK_PACKETS_SENT, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelNetworkPacketsReceivedIsUpdated() throws DeviceNotFoundException {
        final DecimalType mockedValue = new DecimalType(48);
        when(mockedSystemInfo.getNetworkPacketsReceived(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_NETWORK_PACKETS_RECEIVED, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelNetworkNetworkNameIsUpdated() throws DeviceNotFoundException {
        final StringType mockedValue = new StringType("MockN-AQ34");
        when(mockedSystemInfo.getNetworkName(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_NETWORK_NAME, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelNetworkNetworkDisplayNameIsUpdated() throws DeviceNotFoundException {
        final StringType mockedValue = new StringType("Mocked Network Adapter Name");
        when(mockedSystemInfo.getNetworkDisplayName(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_NETWORK_ADAPTER_NAME, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelProcessThreadsIsUpdatedWithPIDse() throws DeviceNotFoundException {
        final int pid = 0; // The pid of the System idle process in Windows is 0
        final DecimalType mockedValue = new DecimalType(4);
        when(mockedSystemInfo.getProcessThreads(pid)).thenReturn(mockedValue);

        initializeThingWithChannelAndPID(CHANNEL_PROCESS_THREADS, "Number", pid);
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelProcessPathIsUpdatedWithPIDset() throws DeviceNotFoundException {
        final int pid = 0; // The pid of the System idle process in Windows is 0
        final StringType mockedValue = new StringType("C:\\Users\\MockedUser\\Process");
        when(mockedSystemInfo.getProcessPath(pid)).thenReturn(mockedValue);

        initializeThingWithChannelAndPID(CHANNEL_PROCESS_PATH, "String", pid);
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelProcessNameIsUpdatedWithPIDset() throws DeviceNotFoundException {
        final int pid = 0; // The pid of the System idle process in Windows is 0
        final StringType mockedValue = new StringType("MockedProcess.exe");
        when(mockedSystemInfo.getProcessName(pid)).thenReturn(mockedValue);

        initializeThingWithChannelAndPID(CHANNEL_PROCESS_NAME, "String", pid);
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelProcessMemoryIsUpdatedWithPIDset() throws DeviceNotFoundException {
        final int pid = 0; // The pid of the System idle process in Windows is 0
        final QuantityType<DataAmount> mockedValue = new QuantityType<>(450, Units.MEBIBYTE);
        when(mockedSystemInfo.getProcessMemoryUsage(pid)).thenReturn(mockedValue);

        initializeThingWithChannelAndPID(CHANNEL_PROCESS_MEMORY, "Number:DataAmount", pid);
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelProcessLoadIsUpdatedWithPIDset() throws DeviceNotFoundException {
        final int pid = 0; // The pid of the System idle process in Windows is 0
        final DecimalType mockedValue = new DecimalType(3);
        when(mockedSystemInfo.getProcessCpuUsage(pid)).thenReturn(mockedValue);

        initializeThingWithChannelAndPID(CHANNEL_PROCESS_LOAD, "Number", pid);
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void testThingHandlesChannelPriorityChange() {
        String priorityKey = "priority";
        String pidKey = "pid";
        String newPriority = "Low";

        String acceptedItemType = "Number";
        initializeThingWithChannel(DEFAULT_TEST_CHANNEL_ID, acceptedItemType);

        final Bridge bridge = systemInfoBridge;
        waitForAssert(() -> assertThat(bridge, is(notNullValue())));

        final Channel channel = bridge.getChannel(DEFAULT_TEST_CHANNEL_ID);
        waitForAssert(() -> assertThat(channel, is(notNullValue())));

        final BridgeHandler handler = bridge.getHandler();
        waitForAssert(() -> assertThat(handler, is(notNullValue())));

        if (!(handler.getClass().equals(SystemInfoComputerHandler.class))) {
            throw new AssertionError("Thing handler not of class SystemInfoHandler");
        }

        final SystemInfoComputerHandler systemInfoHandler = (SystemInfoComputerHandler) handler;
        waitForAssert(() -> {
            assertThat("The initial priority of channel " + channel.getUID() + " is not as expected.",
                    channel.getConfiguration().get(priorityKey), is(equalTo(DEFAULT_CHANNEL_TEST_PRIORITY)));
            assertThat(systemInfoHandler.getHighPriorityChannels().contains(channel.getUID()), is(true));
        });

        // Change the priority of a channel, keep the pid
        Configuration updatedConfig = new Configuration();
        updatedConfig.put(priorityKey, newPriority);
        updatedConfig.put(pidKey, channel.getConfiguration().get(pidKey));
        Channel updatedChannel = ChannelBuilder.create(channel.getUID(), channel.getAcceptedItemType())
                .withType(channel.getChannelTypeUID()).withKind(channel.getKind()).withConfiguration(updatedConfig)
                .build();

        Thing updatedThing = ThingBuilder.create(bridge.getThingTypeUID(), bridge.getUID())
                .withConfiguration(bridge.getConfiguration()).withChannel(updatedChannel).build();

        handler.thingUpdated(updatedThing);

        waitForAssert(() -> {
            assertThat("The prority of the channel was not updated: ", channel.getConfiguration().get(priorityKey),
                    is(equalTo(newPriority)));
            assertThat(systemInfoHandler.getLowPriorityChannels().contains(channel.getUID()), is(true));
        });
    }
}
