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
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.mockito.Mockito.when;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.*;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.openhab.binding.systeminfo.internal.handler.SystemInfoDriveHandler;
import org.openhab.core.config.core.Configuration;
import org.openhab.core.library.dimension.DataAmount;
import org.openhab.core.library.types.DecimalType;
import org.openhab.core.library.types.QuantityType;
import org.openhab.core.library.types.StringType;
import org.openhab.core.library.unit.Units;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusInfo;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.ThingHandler;
import org.openhab.core.thing.binding.builder.ChannelBuilder;
import org.openhab.core.thing.binding.builder.ThingBuilder;
import org.openhab.core.thing.type.ChannelKind;
import org.openhab.core.thing.type.ChannelTypeUID;

import oshi.hardware.HWDiskStore;
import oshi.hardware.HWPartition;
import oshi.hardware.common.AbstractHWDiskStore;

/**
 * OSGi tests for the {@link SystemInfoDriveHandler}
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class SystemInfoDriveHandlerOSGiTest extends SystemInfoOSGiTestBase {

    protected @Nullable Thing systemInfoDeviceThing;

    private static final String TEST_ITEM_NAME = "drive";

    private static final String TEST_DRIVE_NAME = "Mocked Drive Name";
    private static final String TEST_DRIVE_MODEL = "Mocked Drive Model";
    private static final String TEST_DRIVE_SERIAL = "Mocked Drive Serial Number";
    private static final long TEST_DRIVE_READS = 0;
    private static final long TEST_DRIVE_READ_BYTES = 512;
    private static final long TEST_DRIVE_SIZE = 1024;
    private static final long TEST_DRIVE_WRITES = 1;
    private static final long TEST_DRIVE_WRITE_BYTES = 512;

    final HWDiskStore disk = new AbstractHWDiskStore(TEST_DRIVE_NAME, TEST_DRIVE_MODEL, TEST_DRIVE_SERIAL,
            TEST_DRIVE_SIZE) {
        @Override
        public long getReads() {
            return TEST_DRIVE_READS;
        }

        @Override
        public long getReadBytes() {
            return TEST_DRIVE_READ_BYTES;
        }

        @Override
        public long getWrites() {
            return TEST_DRIVE_WRITES;
        }

        @Override
        public long getWriteBytes() {
            return TEST_DRIVE_WRITE_BYTES;
        }

        @Override
        public long getCurrentQueueLength() {
            return 0;
        }

        @Override
        public long getTransferTime() {
            return 0;
        }

        @Override
        @NonNullByDefault({})
        public List<HWPartition> getPartitions() {
            return Collections.emptyList();
        }

        @Override
        public long getTimeStamp() {
            return 0;
        }

        @Override
        public boolean updateAttributes() {
            return true;
        }
    };

    @BeforeEach
    public void setUp() {
        when(mockedSystemInfo.getHardDriveCount()).thenReturn(1);
        when(mockedSystemInfo.getHardDriveList()).thenReturn(List.of(disk));
    }

    @AfterEach
    public void tearDown() {
        final Thing thing = systemInfoDeviceThing;
        if (thing != null) {
            // Remove the systeminfo thing. The handler will also be disposed automatically
            final Thing removedThing = thingRegistry.forceRemove(thing.getUID());
            assertThat("The device thing cannot be deleted", removedThing, is(notNullValue()));

            managedThingProvider.remove(removedThing.getUID());

            final ThingHandler handler = removedThing.getHandler();
            assertThat(handler, is(nullValue()));
        }
    }

    @Test
    public void assertChannelNameIsUpdated() {
        final StringType mockedValue = new StringType(TEST_DRIVE_NAME);

        initializeThingWithChannel(CHANNEL_NAME, CHANNEL_TYPE_NAME, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelReadsIsUpdated() {
        final DecimalType mockedValue = new DecimalType(TEST_DRIVE_READS);

        initializeThingWithChannel(CHANNEL_DRIVE_READS, CHANNEL_TYPE_IO_COUNT, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelReadBytesIsUpdated() {
        final QuantityType<DataAmount> mockedValue = new QuantityType<>(TEST_DRIVE_READ_BYTES, Units.BYTE);

        initializeThingWithChannel(CHANNEL_DRIVE_READ_BYTES, CHANNEL_TYPE_IO_BYTES, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelWritesIsUpdated() {
        final DecimalType mockedValue = new DecimalType(TEST_DRIVE_WRITES);

        initializeThingWithChannel(CHANNEL_DRIVE_WRITES, CHANNEL_TYPE_IO_COUNT, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelWriteBytesIsUpdated() {
        final QuantityType<DataAmount> mockedValue = new QuantityType<>(TEST_DRIVE_WRITE_BYTES, Units.BYTE);

        initializeThingWithChannel(CHANNEL_DRIVE_WRITE_BYTES, CHANNEL_TYPE_IO_BYTES, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    protected void initializeThingWithChannel(final String channelID, final ChannelTypeUID channelTypeUID,
            final String acceptedItemType) {
        Configuration configuration = new Configuration();
        configuration.put(HIGH_PRIORITY_REFRESH_TIME, new BigDecimal(DEFAULT_TEST_INTERVAL_HIGH));
        configuration.put(MEDIUM_PRIORITY_REFRESH_TIME, new BigDecimal(DEFAULT_TEST_INTERVAL_MEDIUM));

        initializeThing(configuration, null, "", DEFAULT_CHANNEL_TEST_PRIORITY, DEFAULT_CHANNEL_PID);

        final Bridge bridge = systemInfoBridge;
        assertThat(bridge, is(notNullValue()));

        final ThingUID thingUID = new ThingUID(THING_TYPE_DRIVE, DEFAULT_TEST_THING_NAME);
        final ThingBuilder thingBuilder = ThingBuilder.create(THING_TYPE_DRIVE, thingUID);
        thingBuilder.withBridge(bridge.getBridgeUID());

        final Configuration thingConfiguration = new Configuration();
        thingConfiguration.put(DEVICE_INDEX_PARAMETER, new BigDecimal(DEFAULT_DEVICE_INDEX));
        thingBuilder.withConfiguration(thingConfiguration);

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
        systemInfoDeviceThing = thing;

        final ThingHandler handler = thing.getHandler();
        assertThat(handler, is(notNullValue()));
        assertThat(handler, is(instanceOf(SystemInfoDriveHandler.class)));
        handler.initialize();

        waitForAssert(() -> {
            final ThingStatusInfo statusInfo = thing.getStatusInfo();
            assertThat("Thing status detail is " + statusInfo.getStatusDetail() + " with description "
                    + statusInfo.getDescription(), thing.getStatus(), is(equalTo(ThingStatus.ONLINE)));
        });

        initializeItem(channelUID, TEST_ITEM_NAME, acceptedItemType);
    }
}
