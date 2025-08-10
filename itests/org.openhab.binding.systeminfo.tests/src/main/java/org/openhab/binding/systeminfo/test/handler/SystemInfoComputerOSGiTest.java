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
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.instanceOf;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.when;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.*;
import static org.openhab.binding.systeminfo.test.SystemInfoOSGiTestConstants.*;

import java.math.BigDecimal;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.openhab.binding.systeminfo.internal.handler.SystemInfoComputerHandler;
import org.openhab.binding.systeminfo.internal.model.DeviceNotFoundException;
import org.openhab.binding.systeminfo.test.data.SystemInfoMockedCentralProcessor;
import org.openhab.binding.systeminfo.test.data.SystemInfoMockedComputerSystem;
import org.openhab.binding.systeminfo.test.data.SystemInfoMockedGlobalMemory;
import org.openhab.binding.systeminfo.test.data.SystemInfoMockedOperatingSystem;
import org.openhab.binding.systeminfo.test.data.SystemInfoMockedSensors;
import org.openhab.binding.systeminfo.test.data.SystemInfoMockedVirtualMemory;
import org.openhab.core.config.core.Configuration;
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
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.BridgeHandler;
import org.openhab.core.thing.binding.builder.BridgeBuilder;
import org.openhab.core.thing.binding.builder.ChannelBuilder;
import org.openhab.core.thing.binding.builder.ThingBuilder;
import org.openhab.core.thing.type.ChannelKind;
import org.openhab.core.thing.type.ChannelTypeUID;
import org.openhab.core.types.UnDefType;

import oshi.hardware.CentralProcessor;
import oshi.hardware.ComputerSystem;
import oshi.hardware.GlobalMemory;
import oshi.hardware.Sensors;
import oshi.software.os.OperatingSystem;

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

    private final ComputerSystem computer = new SystemInfoMockedComputerSystem();
    private final CentralProcessor cpu = new SystemInfoMockedCentralProcessor();
    private final GlobalMemory memory = new SystemInfoMockedGlobalMemory();
    private final OperatingSystem system = new SystemInfoMockedOperatingSystem();
    private final Sensors sensors = new SystemInfoMockedSensors();

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

    @BeforeEach
    public void setUp() {
        lenient().when(mockedSystemInfo.getSystem()).thenReturn(computer);
        lenient().when(mockedSystemInfo.getCPUSpecification()).thenReturn(cpu);
        lenient().when(mockedSystemInfo.getCPUIdentifier()).thenReturn(cpu.getProcessorIdentifier());
        lenient().when(mockedSystemInfo.getMemorySpecifications()).thenReturn(memory);
        lenient().when(mockedSystemInfo.getSwapSpecifications()).thenReturn(memory.getVirtualMemory());
        lenient().when(mockedSystemInfo.getOperatingSystem()).thenReturn(system);
        lenient().when(mockedSystemInfo.getSensors()).thenReturn(sensors);
    }

    @Test
    public void assertInvalidThingConfigurationValuesAreHandled() {
        final var configuration = new Configuration();
        configuration.put(HIGH_PRIORITY_REFRESH_TIME, new BigDecimal(-5)); // invalid value - must be positive
        configuration.put(MEDIUM_PRIORITY_REFRESH_TIME, new BigDecimal(3));

        final var thingUID = new ThingUID(BRIDGE_TYPE_COMPUTER, DEFAULT_TEST_THING_NAME);

        final var builder = BridgeBuilder.create(BRIDGE_TYPE_COMPUTER, thingUID);
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
            final var statusInfo = bridge.getStatusInfo();
            assertThat(statusInfo.getStatusDetail(), is(equalTo(ThingStatusDetail.HANDLER_INITIALIZING_ERROR)));
            assertThat(statusInfo.getDescription(), allOf(notNullValue(), equalTo("@text/offline.cannot-initialize")));
        });

        systemInfoBridge = bridge;
    }

    @Test
    public void assertMediumPriorityChannelIsUpdated() {
        final String channelID = DEFAULT_TEST_CHANNEL_ID;
        final String acceptedItemType = "Number";
        final String priority = "Medium";

        initializeThing(configuration, channelID, acceptedItemType, priority);

        final Bridge bridge = systemInfoBridge;
        if (bridge == null) {
            throw new AssertionError("Bridge is null");
        }

        final var channelUID = new ChannelUID(bridge.getUID(), channelID);
        initializeItem(channelUID, TEST_ITEM_NAME, acceptedItemType);
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
    public void assertChannelCpuDescriptionIsUpdated() {
        final String format = "Model: %s %s,family: %s, vendor: %s, sn: %s, identifier: %s ";
        final var mockedValue = new StringType(String.format(format, SystemInfoMockedCentralProcessor.TEST_CPU_MODEL,
                SystemInfoMockedCentralProcessor.TEST_CPU_IS_64_BIT ? "64 bit" : "32 bit",
                SystemInfoMockedCentralProcessor.TEST_CPU_FAMILY, SystemInfoMockedCentralProcessor.TEST_CPU_VENDOR,
                SystemInfoMockedComputerSystem.TEST_SYSTEM_SERIAL,
                mockedSystemInfo.getCPUIdentifier().getIdentifier()));

        initializeBridgeWithChannel(CHANNEL_DESCRIPTION, CHANNEL_CPU_GROUP, CHANNEL_TYPE_NAME, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelCpuFrequency() {
        final var mockedValue = new QuantityType<>(SystemInfoMockedCentralProcessor.TEST_LOGICAL_CPU_FREQUENCY,
                Units.HERTZ);

        initializeBridgeWithChannel(CHANNEL_CPU_FREQUENCY, CHANNEL_CPU_GROUP, CHANNEL_TYPE_FREQUENCY,
                "Number:Frequency");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelCpuMaxFrequencyIsUpdated() {
        final var mockedValue = new QuantityType<>(SystemInfoMockedCentralProcessor.TEST_CPU_MAX_FREQUENCY,
                Units.HERTZ);

        initializeBridgeWithChannel(CHANNEL_CPU_MAXFREQUENCY, CHANNEL_CPU_GROUP, CHANNEL_TYPE_MAX_FREQUENCY,
                "Number:Frequency");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelCpuNameIsUpdated() {
        final var mockedValue = new StringType(SystemInfoMockedCentralProcessor.TEST_CPU_NAME);

        initializeBridgeWithChannel(CHANNEL_NAME, CHANNEL_CPU_GROUP, CHANNEL_TYPE_NAME, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelCpuTemperatureIsUpdated() {
        final var mockedValue = new QuantityType<>(SystemInfoMockedSensors.TEST_CPU_TEMPERATURE, SIUnits.CELSIUS);

        initializeBridgeWithChannel(CHANNEL_CPU_TEMPERATURE, CHANNEL_CPU_GROUP, CHANNEL_TYPE_TEMPERATURE,
                "Number:Temperature");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelCpuVoltageIsUpdated() {
        final var mockedValue = new QuantityType<>(SystemInfoMockedSensors.TEST_CPU_VOLTAGE, Units.VOLT);

        initializeBridgeWithChannel(CHANNEL_CPU_VOLTAGE, CHANNEL_CPU_GROUP, CHANNEL_TYPE_VOLTAGE,
                "Number:ElectricPotential");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelMemoryAvailableIsUpdated() {
        final var mockedValue = new QuantityType<>(SystemInfoMockedGlobalMemory.TEST_MEMORY_AVAILABLE, Units.BYTE);

        initializeBridgeWithChannel(CHANNEL_AVAILABLE, CHANNEL_MEMORY_GROUP, CHANNEL_TYPE_BYTES, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelMemoryAvailablePercentIsUpdated() {
        final double scale = Math.pow(10, PRECISION_AFTER_DECIMAL_SIGN);
        final double percent = (double) SystemInfoMockedGlobalMemory.TEST_MEMORY_AVAILABLE
                / (double) SystemInfoMockedGlobalMemory.TEST_MEMORY_TOTAL * 100.0;
        final var mockedValue = new QuantityType<>(BigDecimal.valueOf((double) Math.round(percent * scale) / scale),
                Units.PERCENT);

        initializeBridgeWithChannel(CHANNEL_AVAILABLE_PERCENT, CHANNEL_MEMORY_GROUP, CHANNEL_TYPE_PERCENT,
                "Number:Dimensionless");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelMemoryTotalIsUpdated() {
        final var mockedValue = new QuantityType<>(SystemInfoMockedGlobalMemory.TEST_MEMORY_TOTAL, Units.BYTE);

        initializeBridgeWithChannel(CHANNEL_TOTAL, CHANNEL_MEMORY_GROUP, CHANNEL_TYPE_TOTAL, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelMemoryUsedIsUpdated() {
        final var mockedValue = new QuantityType<>(SystemInfoMockedGlobalMemory.TEST_MEMORY_USED, Units.BYTE);

        initializeBridgeWithChannel(CHANNEL_USED, CHANNEL_MEMORY_GROUP, CHANNEL_TYPE_BYTES, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelMemoryUsedPercentIsUpdated() {
        final double scale = Math.pow(10, PRECISION_AFTER_DECIMAL_SIGN);
        final double percent = (double) SystemInfoMockedGlobalMemory.TEST_MEMORY_USED
                / (double) SystemInfoMockedGlobalMemory.TEST_MEMORY_TOTAL * 100.0;
        final var mockedValue = new QuantityType<>(BigDecimal.valueOf((double) Math.round(percent * scale) / scale),
                Units.PERCENT);

        initializeBridgeWithChannel(CHANNEL_USED_PERCENT, CHANNEL_MEMORY_GROUP, CHANNEL_TYPE_PERCENT,
                "Number:Dimensionless");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSystemLoadIsUpdated() {
        final var mockedValue = new QuantityType<>(SystemInfoMockedCentralProcessor.TEST_CPU_LOAD, Units.PERCENT);

        initializeBridgeWithChannel(CHANNEL_LOAD, CHANNEL_SYSTEM_GROUP, CHANNEL_TYPE_LOAD, "Number:Dimensionless");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSystemLoad1IsUpdated() {
        final var mockedValue = new DecimalType(SystemInfoMockedCentralProcessor.TEST_CPU_LOAD1);

        initializeBridgeWithChannel(CHANNEL_SYSTEM_LOAD_1, CHANNEL_SYSTEM_GROUP, CHANNEL_TYPE_LOAD_AVERAGE, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSystemLoad5IsUpdated() {
        final var mockedValue = new DecimalType(SystemInfoMockedCentralProcessor.TEST_CPU_LOAD5);

        initializeBridgeWithChannel(CHANNEL_SYSTEM_LOAD_5, CHANNEL_SYSTEM_GROUP, CHANNEL_TYPE_LOAD_AVERAGE, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSystemLoad15IsUpdated() {
        final var mockedValue = new DecimalType(SystemInfoMockedCentralProcessor.TEST_CPU_LOAD15);

        initializeBridgeWithChannel(CHANNEL_SYSTEM_LOAD_15, CHANNEL_SYSTEM_GROUP, CHANNEL_TYPE_LOAD_AVERAGE, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSystemThreadsIsUpdated() {
        final var mockedValue = new DecimalType(SystemInfoMockedOperatingSystem.TEST_SYSTEM_THREAD_COUNT);

        initializeBridgeWithChannel(CHANNEL_THREADS, CHANNEL_SYSTEM_GROUP, CHANNEL_TYPE_THREADS, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSystemUptimeIsUpdated() {
        final var mockedValue = new QuantityType<>(SystemInfoMockedOperatingSystem.TEST_SYSTEM_UPTIME, Units.SECOND);

        initializeBridgeWithChannel(CHANNEL_SYSTEM_UPTIME, CHANNEL_SYSTEM_GROUP, CHANNEL_TYPE_UPTIME, "Number:Time");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSwapAvailableIsUpdated() {
        final var mockedValue = new QuantityType<>(SystemInfoMockedVirtualMemory.TEST_SWAP_AVAILABLE, Units.BYTE);

        initializeBridgeWithChannel(CHANNEL_AVAILABLE, CHANNEL_SWAP_GROUP, CHANNEL_TYPE_BYTES, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSwapAvailablePercentIsUpdated() {
        final double scale = Math.pow(10, PRECISION_AFTER_DECIMAL_SIGN);
        final double percent = (double) SystemInfoMockedVirtualMemory.TEST_SWAP_AVAILABLE
                / (double) SystemInfoMockedVirtualMemory.TEST_SWAP_TOTAL * 100.0;
        final var mockedValue = new QuantityType<>(BigDecimal.valueOf((double) Math.round(percent * scale) / scale),
                Units.PERCENT);

        initializeBridgeWithChannel(CHANNEL_AVAILABLE_PERCENT, CHANNEL_SWAP_GROUP, CHANNEL_TYPE_PERCENT,
                "Number:Dimensionless");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSwapTotalIsUpdated() {
        final var mockedValue = new QuantityType<>(SystemInfoMockedVirtualMemory.TEST_SWAP_TOTAL, Units.BYTE);

        initializeBridgeWithChannel(CHANNEL_TOTAL, CHANNEL_SWAP_GROUP, CHANNEL_TYPE_TOTAL, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSwapUsedIsUpdated() {
        final var mockedValue = new QuantityType<>(SystemInfoMockedVirtualMemory.TEST_SWAP_USED, Units.BYTE);

        initializeBridgeWithChannel(CHANNEL_USED, CHANNEL_SWAP_GROUP, CHANNEL_TYPE_BYTES, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSwapUsedPercentIsUpdated() {
        final double scale = Math.pow(10, PRECISION_AFTER_DECIMAL_SIGN);
        final double percent = (double) SystemInfoMockedVirtualMemory.TEST_SWAP_USED
                / (double) SystemInfoMockedVirtualMemory.TEST_SWAP_TOTAL * 100.0;
        final var mockedValue = new QuantityType<>(BigDecimal.valueOf((double) Math.round(percent * scale) / scale),
                Units.PERCENT);

        initializeBridgeWithChannel(CHANNEL_USED_PERCENT, CHANNEL_SWAP_GROUP, CHANNEL_TYPE_PERCENT,
                "Number:Dimensionless");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSensorsFanSpeedIsUpdated() throws DeviceNotFoundException {
        final var mockedValue = new DecimalType(180);
        when(mockedSystemInfo.getSensorsFanSpeed(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_SENSORS_FAN_SPEED, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelBatteryNameIsUpdated() throws DeviceNotFoundException {
        final var mockedValue = new StringType("Mocked Battery Name");
        when(mockedSystemInfo.getBatteryName(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_BATTERY_NAME, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelBatteryRemainingCapacityIsUpdated() throws DeviceNotFoundException {
        final var mockedValue = new PercentType(20);
        when(mockedSystemInfo.getBatteryRemainingCapacity(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_BATTERY_REMAINING_CAPACITY, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelBatteryRemainingTimeIsUpdated() throws DeviceNotFoundException {
        final var mockedValue = new QuantityType<>(3600, Units.MINUTE);
        when(mockedSystemInfo.getBatteryRemainingTime(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_BATTERY_REMAINING_TIME, "Number:Time");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelDisplayInformationIsUpdated() throws DeviceNotFoundException {
        final var mockedValue = new StringType("Mocked Display Information");
        when(mockedSystemInfo.getDisplayInformation(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);

        initializeThingWithChannel(CHANNEL_DISPLAY_INFORMATION, "String");
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
        final var updatedConfig = new Configuration();
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

    protected void initializeBridgeWithChannel(final String channelID, @Nullable String groupID,
            final ChannelTypeUID channelTypeUID, final String acceptedItemType) {
        final var thingUID = new ThingUID(BRIDGE_TYPE_COMPUTER, DEFAULT_TEST_THING_NAME);

        final var builder = BridgeBuilder.create(BRIDGE_TYPE_COMPUTER, thingUID);
        builder.withConfiguration(configuration);

        final ChannelUID channelUID;
        if (groupID == null) {
            channelUID = new ChannelUID(thingUID, channelID);
        } else {
            channelUID = new ChannelUID(thingUID, groupID, channelID);
        }
        final var channelBuilder = ChannelBuilder.create(channelUID, acceptedItemType);
        channelBuilder.withType(channelTypeUID).withKind(ChannelKind.STATE);

        final var channelConfiguration = new Configuration();
        channelConfiguration.put(PRIORITY_PARAMETER, DEFAULT_CHANNEL_TEST_PRIORITY);
        channelBuilder.withConfiguration(channelConfiguration);
        builder.withChannel(channelBuilder.build());

        final Bridge bridge = builder.build();
        managedThingProvider.add(bridge);

        final BridgeHandler handler = bridge.getHandler();
        if (handler == null) {
            throw new AssertionError("Bridge handler is null");
        }
        assertThat(handler, is(instanceOf(SystemInfoComputerHandler.class)));
        handler.initialize();

        waitForAssert(() -> {
            final var statusInfo = bridge.getStatusInfo();
            assertThat(String.format("Bridge status detail is %s with description %s", statusInfo.getStatusDetail(),
                    statusInfo.getDescription()), bridge.getStatus(), is(equalTo(ThingStatus.ONLINE)));
        });

        systemInfoBridge = bridge;

        initializeItem(channelUID, TEST_ITEM_NAME, acceptedItemType);
    }
}
