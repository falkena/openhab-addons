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
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.when;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.BRIDGE_TYPE_DRIVE;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_DESCRIPTION;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NAME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_PARTITION_GROUP;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_TYPE;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.DEVICE_INDEX_PARAMETER;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.PRIORITY_PARAMETER;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.THING_TYPE_PARTITION;
import static org.openhab.binding.systeminfo.test.SystemInfoOSGiTestConstants.CHANNEL_TYPE_DESCRIPTION;
import static org.openhab.binding.systeminfo.test.SystemInfoOSGiTestConstants.CHANNEL_TYPE_NAME;
import static org.openhab.binding.systeminfo.test.SystemInfoOSGiTestConstants.CHANNEL_TYPE_TYPE;
import static org.openhab.core.thing.ChannelUID.CHANNEL_GROUP_SEPARATOR;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.openhab.binding.systeminfo.internal.handler.SystemInfoDriveHandler;
import org.openhab.binding.systeminfo.internal.handler.SystemInfoPartitionHandler;
import org.openhab.binding.systeminfo.test.data.SystemInfoMockedHWDiskStore;
import org.openhab.binding.systeminfo.test.data.SystemInfoMockedOSFileStore;
import org.openhab.core.config.core.Configuration;
import org.openhab.core.library.types.StringType;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusInfo;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.BridgeHandler;
import org.openhab.core.thing.binding.ThingHandler;
import org.openhab.core.thing.binding.builder.BridgeBuilder;
import org.openhab.core.thing.binding.builder.ChannelBuilder;
import org.openhab.core.thing.binding.builder.ThingBuilder;
import org.openhab.core.thing.type.ChannelKind;
import org.openhab.core.thing.type.ChannelTypeUID;

import oshi.hardware.HWDiskStore;
import oshi.software.os.OSFileStore;

/**
 * OSGi tests for the {@link SystemInfoPartitionHandler}
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
@ExtendWith(MockitoExtension.class)
public class SystemInfoPartitionHandlerOSGiTest extends SystemInfoDeviceHandlerOSGiTestBase {
    private static final String TEST_ITEM_NAME = "partition";

    private @Nullable Bridge bridge;
    private @Nullable Thing thing;
    private final HWDiskStore disk = new SystemInfoMockedHWDiskStore();
    private final List<OSFileStore> storage = List.of(new SystemInfoMockedOSFileStore());

    @BeforeEach
    public void setUp() {
        lenient().when(mockedSystemInfo.getHardDriveCount()).thenReturn(1);
        when(mockedSystemInfo.getHardDriveList()).thenReturn(List.of(disk));
        lenient().when(mockedSystemInfo.getFileStorageCount()).thenReturn(1);
        when(mockedSystemInfo.getFileStorageList()).thenReturn(storage);
    }

    @AfterEach
    public void tearDown() {
        final Thing thing = this.thing;
        this.thing = null;

        if (thing != null) {
            // Remove the partition thing. The handler will also be disposed automatically
            final Thing removedThing = thingRegistry.forceRemove(thing.getUID());
            if (removedThing == null) {
                throw new AssertionError("The device thing cannot be deleted");
            }

            managedThingProvider.remove(removedThing.getUID());

            final ThingHandler handler = removedThing.getHandler();
            assertThat(handler, is(nullValue()));
        }

        final Bridge bridge = this.bridge;
        this.bridge = null;

        if (bridge != null) {
            // Remove the drive bridge. The handler will also be disposed automatically
            final Thing removedThing = thingRegistry.forceRemove(bridge.getUID());
            if (removedThing == null) {
                throw new AssertionError("The device thing cannot be deleted");
            }

            managedThingProvider.remove(removedThing.getUID());

            final ThingHandler handler = removedThing.getHandler();
            assertThat(handler, is(nullValue()));
        }
    }

    /*
     * if (channelGroupId.contains(CHANNEL_VOLUME_GROUP)) {
     * final OSFileStore volume = volumes.get(channelGroupId);
     * if ((volume != null) && volume.updateAttributes()) {
     * final State state = switch (channelUID.getIdWithoutGroup()) {
     * case CHANNEL_NAME -> new StringType(volume.getName());
     * case CHANNEL_DESCRIPTION -> new StringType(volume.getDescription());
     * case CHANNEL_TYPE -> new StringType(volume.getType());
     * case CHANNEL_AVAILABLE, CHANNEL_AVAILABLE_PERCENT ->
     * getChannelState(channelUID, volume.getFreeSpace(),
     * volume.getTotalSpace() - volume.getFreeSpace(), volume.getTotalSpace());
     * case CHANNEL_TOTAL -> getChannelState(channelUID, volume.getFreeSpace(),
     * volume.getTotalSpace() - volume.getFreeSpace(), volume.getTotalSpace());
     * case CHANNEL_USED, CHANNEL_USED_PERCENT ->
     * getChannelState(channelUID, volume.getFreeSpace(),
     * volume.getTotalSpace() - volume.getFreeSpace(), volume.getTotalSpace());
     * default -> UnDefType.UNDEF;
     * };
     */
    @Test
    public void assertChannelPartitionNameIsUpdated() {
        final StringType mockedValue = new StringType(SystemInfoMockedHWDiskStore.TEST_PARTITION_NAME);

        final String channelID = CHANNEL_PARTITION_GROUP + CHANNEL_GROUP_SEPARATOR + CHANNEL_NAME;
        initializeThingWithChannel(channelID, CHANNEL_TYPE_NAME, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelPartitionDescriptionIsUpdated() {
        final StringType mockedValue = new StringType(SystemInfoMockedHWDiskStore.TEST_PARTITION_IDENTIFICATION);

        final String channelID = CHANNEL_PARTITION_GROUP + CHANNEL_GROUP_SEPARATOR + CHANNEL_DESCRIPTION;
        initializeThingWithChannel(channelID, CHANNEL_TYPE_DESCRIPTION, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelPartitionTypeIsUpdated() {
        final StringType mockedValue = new StringType(SystemInfoMockedHWDiskStore.TEST_PARTITION_TYPE);

        final String channelID = CHANNEL_PARTITION_GROUP + CHANNEL_GROUP_SEPARATOR + CHANNEL_TYPE;
        initializeThingWithChannel(channelID, CHANNEL_TYPE_TYPE, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    /*
     * @Test
     * public void assertChannelStorageAvailablePercentIsUpdated() throws DeviceNotFoundException {
     * PercentType mockedValue = new PercentType(20);
     * when(mockedSystemInfo.getStorageAvailablePercent(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);
     * 
     * initializeBridgeWithChannel(CHANNEL_STORAGE_AVAILABLE_PERCENT, null, CHANNEL_TYPE_PERCENT, "Number");
     * assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
     * }
     * 
     * @Test
     * public void assertChannelStorageTotalIsUpdated() throws DeviceNotFoundException {
     * QuantityType<DataAmount> mockedStorageTotalValue = new QuantityType<>(2500, Units.BYTE);
     * when(mockedSystemInfo.getStorageTotal(DEFAULT_DEVICE_INDEX)).thenReturn(mockedStorageTotalValue);
     * 
     * initializeBridgeWithChannel(CHANNEL_STORAGE_TOTAL, null, CHANNEL_TYPE_TOTAL, "Number:DataAmount");
     * assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedStorageTotalValue);
     * }
     * 
     * @Test
     * public void assertChannelStorageUsedIsUpdated() throws DeviceNotFoundException {
     * QuantityType<DataAmount> mockedStorageUsedValue = new QuantityType<>(500, Units.BYTE);
     * when(mockedSystemInfo.getStorageUsed(DEFAULT_DEVICE_INDEX)).thenReturn(mockedStorageUsedValue);
     * 
     * initializeBridgeWithChannel(CHANNEL_STORAGE_USED, null, CHANNEL_TYPE_MEMORY_BYTES, "Number:DataAmount");
     * assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedStorageUsedValue);
     * }
     * 
     * @Test
     * public void assertChannelStorageUsedPercentIsUpdated() throws DeviceNotFoundException {
     * PercentType mockedValue = new PercentType(20);
     * when(mockedSystemInfo.getStorageUsedPercent(DEFAULT_DEVICE_INDEX)).thenReturn(mockedValue);
     * 
     * initializeBridgeWithChannel(CHANNEL_STORAGE_USED_PERCENT, null, CHANNEL_TYPE_PERCENT, "Number");
     * assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
     * }
     * 
     * @Test
     * public void assertChannelStorageTypeIsUpdated() throws DeviceNotFoundException {
     * String channnelID = SystemInfoBindingConstants.CHANNEL_STORAGE_TYPE;
     * 
     * StringType mockedStorageType = new StringType("Mocked Storage Type");
     * when(mockedSystemInfo.getStorageType(DEFAULT_DEVICE_INDEX)).thenReturn(mockedStorageType);
     * 
     * initializeThingWithChannel(channnelID, "String");
     * assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedStorageType);
     * }
     */
    private void initializeThingWithChannel(final String channelID, final ChannelTypeUID channelTypeUID,
            final String acceptedItemType) {
        initializeThing(configuration, null, "", DEFAULT_CHANNEL_TEST_PRIORITY, DEFAULT_CHANNEL_PID);

        final Bridge systemInfoBridge = this.systemInfoBridge;
        if (systemInfoBridge == null) {
            throw new AssertionError("Bridge is null");
        }

        final Map<String, Object> config = Map.of(DEVICE_INDEX_PARAMETER, BigDecimal.valueOf(DEFAULT_DEVICE_INDEX));

        final ThingUID deviceUID = new ThingUID(BRIDGE_TYPE_DRIVE, DEFAULT_TEST_THING_NAME);
        final BridgeBuilder deviceBuilder = BridgeBuilder.create(BRIDGE_TYPE_DRIVE, deviceUID);
        deviceBuilder.withConfiguration(new Configuration(config));
        deviceBuilder.withBridge(systemInfoBridge.getUID());

        final Bridge bridge = deviceBuilder.build();
        assertThat(bridge, is(notNullValue()));
        managedThingProvider.add(bridge);

        final BridgeHandler bridgeHandler = bridge.getHandler();
        if (bridgeHandler == null) {
            throw new AssertionError("SystemInfoDriveHandler is null");
        }
        assertThat(bridgeHandler, is(instanceOf(SystemInfoDriveHandler.class)));
        bridgeHandler.initialize();

        waitForAssert(() -> {
            final Thing registered = bridgeHandler.getThing();
            final ThingStatusInfo statusInfo = registered.getStatusInfo();
            assertThat(String.format("Thing status detail is %s with description %s", statusInfo.getStatusDetail(),
                    statusInfo.getDescription()), registered.getStatus(), is(equalTo(ThingStatus.ONLINE)));
        });

        final ThingUID thingUID = new ThingUID(THING_TYPE_PARTITION, DEFAULT_TEST_THING_NAME);
        final ThingBuilder thingBuilder = ThingBuilder.create(THING_TYPE_PARTITION, thingUID);
        thingBuilder.withConfiguration(new Configuration(config));
        thingBuilder.withBridge(bridge.getUID());

        final ChannelUID channelUID = new ChannelUID(thingUID, channelID);
        final ChannelBuilder channelBuilder = ChannelBuilder.create(channelUID, acceptedItemType);
        channelBuilder.withType(channelTypeUID).withKind(ChannelKind.STATE);

        Configuration channelConfiguration = new Configuration();
        channelConfiguration.put(PRIORITY_PARAMETER, DEFAULT_CHANNEL_TEST_PRIORITY);
        channelBuilder.withConfiguration(channelConfiguration);
        thingBuilder.withChannel(channelBuilder.build());

        final Thing thing = thingBuilder.build();
        assertThat(thing, is(notNullValue()));
        managedThingProvider.add(thing);

        final ThingHandler thingHandler = thing.getHandler();
        if (thingHandler == null) {
            throw new AssertionError("SystemInfoPartitionHandler is null");
        }
        assertThat(thingHandler, is(instanceOf(SystemInfoPartitionHandler.class)));
        thingHandler.initialize();

        waitForAssert(() -> {
            final ThingStatusInfo statusInfo = thing.getStatusInfo();
            assertThat(String.format("Thing status detail is %s with description %s", statusInfo.getStatusDetail(),
                    statusInfo.getDescription()), thing.getStatus(), is(equalTo(ThingStatus.ONLINE)));
        });

        this.bridge = bridge;
        this.thing = thing;

        initializeItem(channelUID, TEST_ITEM_NAME, acceptedItemType);
    }
}
