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
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.when;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_DESCRIPTION;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NAME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NETWORK_IP;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NETWORK_MAC;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NETWORK_RECEIVED;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NETWORK_RECEIVED_BYTES;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NETWORK_SENT;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NETWORK_SENT_BYTES;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.DEVICE_NAME_PARAMETER;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.THING_TYPE_NETWORK;
import static org.openhab.binding.systeminfo.test.SystemInfoOSGiTestConstants.CHANNEL_TYPE_BYTES;
import static org.openhab.binding.systeminfo.test.SystemInfoOSGiTestConstants.CHANNEL_TYPE_COUNT;
import static org.openhab.binding.systeminfo.test.SystemInfoOSGiTestConstants.CHANNEL_TYPE_DESCRIPTION;
import static org.openhab.binding.systeminfo.test.SystemInfoOSGiTestConstants.CHANNEL_TYPE_IP;
import static org.openhab.binding.systeminfo.test.SystemInfoOSGiTestConstants.CHANNEL_TYPE_MAC;
import static org.openhab.binding.systeminfo.test.SystemInfoOSGiTestConstants.CHANNEL_TYPE_NAME;

import java.util.List;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.openhab.binding.systeminfo.internal.handler.SystemInfoNetworkHandler;
import org.openhab.binding.systeminfo.test.data.SystemInfoMockedNetworkInterface;
import org.openhab.core.config.core.Configuration;
import org.openhab.core.library.dimension.DataAmount;
import org.openhab.core.library.types.DecimalType;
import org.openhab.core.library.types.QuantityType;
import org.openhab.core.library.types.StringType;
import org.openhab.core.library.unit.Units;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.ThingHandler;
import org.openhab.core.thing.type.ChannelTypeUID;

import oshi.hardware.NetworkIF;

/**
 * OSGi tests for the {@link SystemInfoNetworkHandler}
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
@ExtendWith(MockitoExtension.class)
public class SystemInfoNetworkHandlerOSGiTest extends SystemInfoDeviceHandlerOSGiTestBase {

    private static final String TEST_ITEM_NAME = "network";
    private final SystemInfoMockedNetworkInterface adapter = new SystemInfoMockedNetworkInterface();
    /*
     * @Test
     * public void assertStateOfSecondDeviceIsUpdated() {
     * // This test assumes that at least 2 network interfaces are present on the test platform
     * initializeThingWithChannel(String.format("network%d#mac", 1), "String");
     * assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, UnDefType.UNDEF);
     * }
     */

    @BeforeEach
    public void setUp() {
        lenient().when(mockedSystemInfo.getNetworkInterfaceCount()).thenReturn(1);
        when(mockedSystemInfo.getNetworkInterfaceList()).thenReturn(List.of(adapter));
    }

    @Test
    public void assertMockDataConsistency() {
        final List<NetworkIF> adapters = mockedSystemInfo.getNetworkInterfaceList();
        assertThat(mockedSystemInfo.getNetworkInterfaceCount(), is(equalTo(adapters.size())));
    }

    @Test
    public void assertChannelNameIsUpdated() {
        final StringType mockedValue = new StringType(SystemInfoMockedNetworkInterface.TEST_NETWORK_NAME);

        initializeThingWithChannel(CHANNEL_NAME, CHANNEL_TYPE_NAME, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelDescriptionIsUpdated() {
        final StringType mockedValue = new StringType(SystemInfoMockedNetworkInterface.TEST_NETWORK_DESCRIPTION);

        initializeThingWithChannel(CHANNEL_DESCRIPTION, CHANNEL_TYPE_DESCRIPTION, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelReceivedIsUpdated() {
        final DecimalType mockedValue = new DecimalType(SystemInfoMockedNetworkInterface.TEST_NETWORK_RECEIVED);

        initializeThingWithChannel(CHANNEL_NETWORK_RECEIVED, CHANNEL_TYPE_COUNT, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelReceivedBytesIsUpdated() {
        final QuantityType<DataAmount> mockedValue = new QuantityType<>(
                SystemInfoMockedNetworkInterface.TEST_NETWORK_RECEIVED_BYTES, Units.BYTE);

        initializeThingWithChannel(CHANNEL_NETWORK_RECEIVED_BYTES, CHANNEL_TYPE_BYTES, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSentIsUpdated() {
        final DecimalType mockedValue = new DecimalType(SystemInfoMockedNetworkInterface.TEST_NETWORK_SENT);

        initializeThingWithChannel(CHANNEL_NETWORK_SENT, CHANNEL_TYPE_COUNT, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelSentBytesIsUpdated() {
        final QuantityType<DataAmount> mockedValue = new QuantityType<>(
                SystemInfoMockedNetworkInterface.TEST_NETWORK_SENT_BYTES, Units.BYTE);

        initializeThingWithChannel(CHANNEL_NETWORK_SENT_BYTES, CHANNEL_TYPE_BYTES, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelIpIsUpdated() {
        final StringType mockedValue = new StringType(SystemInfoMockedNetworkInterface.TEST_NETWORK_IP);

        initializeThingWithChannel(CHANNEL_NETWORK_IP, CHANNEL_TYPE_IP, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelMacIsUpdated() {
        final StringType mockedValue = new StringType(SystemInfoMockedNetworkInterface.TEST_NETWORK_MAC);

        initializeThingWithChannel(CHANNEL_NETWORK_MAC, CHANNEL_TYPE_MAC, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    private void initializeThingWithChannel(final String channelID, final ChannelTypeUID channelTypeUID,
            final String acceptedItemType) {

        final Configuration thingConfiguration = new Configuration();
        thingConfiguration.put(DEVICE_NAME_PARAMETER, SystemInfoMockedNetworkInterface.TEST_NETWORK_NAME);

        final ThingUID thingUID = new ThingUID(THING_TYPE_NETWORK, DEFAULT_TEST_THING_NAME);
        final ChannelUID channelUID = new ChannelUID(thingUID, channelID);
        final Thing thing = initializeThingWithChannel(thingUID, THING_TYPE_NETWORK, thingConfiguration, channelUID,
                channelTypeUID, acceptedItemType);

        final ThingHandler handler = thing.getHandler();
        if (handler == null) {
            throw new AssertionError("SystemInfoNetworkHandler is null");
        }
        assertThat(handler, is(instanceOf(SystemInfoNetworkHandler.class)));

        initializeItem(channelUID, TEST_ITEM_NAME, acceptedItemType);
    }
}
