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

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.mockito.Mockito.lenient;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.*;
import static org.openhab.binding.systeminfo.test.SystemInfoOSGiTestConstants.*;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.openhab.binding.systeminfo.internal.config.SystemInfoProcessConfig;
import org.openhab.binding.systeminfo.internal.handler.SystemInfoProcessHandler;
import org.openhab.binding.systeminfo.test.data.SystemInfoMockedOSProcess;
import org.openhab.binding.systeminfo.test.data.SystemInfoMockedOperatingSystem;
import org.openhab.core.config.core.Configuration;
import org.openhab.core.library.types.DecimalType;
import org.openhab.core.library.types.QuantityType;
import org.openhab.core.library.types.StringType;
import org.openhab.core.library.unit.Units;
import org.openhab.core.thing.binding.ThingHandler;
import org.openhab.core.thing.binding.builder.ChannelBuilder;
import org.openhab.core.thing.binding.builder.ThingBuilder;
import org.openhab.core.thing.type.ChannelKind;
import org.openhab.core.thing.type.ChannelTypeUID;

import oshi.software.os.OperatingSystem;

/**
 * OSGi tests for the {@link SystemInfoProcessHandler}
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
@ExtendWith(MockitoExtension.class)
public class SystemInfoProcessHandlerOSGiTest extends SystemInfoOSGiTestBase {
    private static final String TEST_ITEM_NAME = "process";

    private @Nullable Thing thing;
    private final OperatingSystem system = new SystemInfoMockedOperatingSystem();

    @BeforeEach
    @Override
    public void initialize() {
        super.initialize();

        lenient().when(mockedSystemInfo.getOperatingSystem()).thenReturn(system);
    }

    @AfterEach
    @Override
    public void dispose() {
        final Thing thing = this.thing;
        this.thing = null;

        if (thing != null) {
            // Remove the process thing. The handler will also be disposed automatically
            final Thing removedThing = thingRegistry.forceRemove(thing.getUID());
            if (removedThing == null) {
                throw new AssertionError("The device thing cannot be deleted");
            }

            managedThingProvider.remove(removedThing.getUID());

            final ThingHandler handler = removedThing.getHandler();
            assertThat(handler, is(nullValue()));
        }

        super.dispose();
    }

    @Test
    public void assertChannelLoadIsUpdated() {
        final var mockedValue = new QuantityType<>(SystemInfoMockedOSProcess.TEST_PROCESS_LOAD, Units.PERCENT);

        initializeThingWithChannel(CHANNEL_LOAD, CHANNEL_TYPE_LOAD, "Number:Dimensionless");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelNameIsUpdated() {
        final var mockedValue = new StringType(SystemInfoMockedOSProcess.TEST_PROCESS_NAME);

        initializeThingWithChannel(CHANNEL_NAME, CHANNEL_TYPE_NAME, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelPathIsUpdated() {
        final var mockedValue = new StringType(SystemInfoMockedOSProcess.TEST_PROCESS_PATH);

        initializeThingWithChannel(CHANNEL_PROCESS_PATH, CHANNEL_TYPE_PATH, "String");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelResidentMemoryIsUpdated() {
        final var mockedValue = new QuantityType<>(SystemInfoMockedOSProcess.TEST_PROCESS_RESIDENT_MEMORY, Units.BYTE);

        initializeThingWithChannel(CHANNEL_USED, CHANNEL_TYPE_BYTES, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelThreadCountIsUpdated() {
        final var mockedValue = new DecimalType(SystemInfoMockedOSProcess.TEST_PROCESS_THREAD_COUNT);

        initializeThingWithChannel(CHANNEL_THREADS, CHANNEL_TYPE_THREADS, "Number");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    @Test
    public void assertChannelVirtualMemoryIsUpdated() {
        final var mockedValue = new QuantityType<>(SystemInfoMockedOSProcess.TEST_PROCESS_VIRTUAL_MEMORY, Units.BYTE);

        initializeThingWithChannel(CHANNEL_ALLOCATED, CHANNEL_TYPE_BYTES, "Number:DataAmount");
        assertItemState(TEST_ITEM_NAME, DEFAULT_CHANNEL_TEST_PRIORITY, mockedValue);
    }

    private void initializeThingWithChannel(final String channelID, final ChannelTypeUID channelTypeUID,
            final String acceptedItemType) {
        initializeThing(configuration, null, "", DEFAULT_CHANNEL_TEST_PRIORITY);

        final var bridge = systemInfoBridge;
        if (bridge == null) {
            throw new AssertionError("Bridge is null");
        }

        final SystemInfoProcessConfig thingConfig = new SystemInfoProcessConfig();
        thingConfig.setProcessID(SystemInfoMockedOSProcess.TEST_PROCESS_ID);
        thingConfig.setProcessName(SystemInfoMockedOSProcess.TEST_PROCESS_NAME);

        final var thingUID = new ThingUID(THING_TYPE_PROCESS, DEFAULT_TEST_THING_NAME);
        final var thingBuilder = ThingBuilder.create(THING_TYPE_PROCESS, thingUID);
        thingBuilder.withBridge(bridge.getUID()).withConfiguration(thingConfig.asConfiguration());

        final var channelUID = new ChannelUID(thingUID, channelID);
        final var channelBuilder = ChannelBuilder.create(channelUID, acceptedItemType);
        channelBuilder.withType(channelTypeUID).withKind(ChannelKind.STATE);

        final var channelConfiguration = new Configuration();
        channelConfiguration.put(PRIORITY_PARAMETER, DEFAULT_CHANNEL_TEST_PRIORITY);
        channelBuilder.withConfiguration(channelConfiguration);
        thingBuilder.withChannel(channelBuilder.build());

        final Thing thing = thingBuilder.build();
        assertThat(thing, is(notNullValue()));
        managedThingProvider.add(thing);

        final ThingHandler handler = thing.getHandler();
        if (handler == null) {
            throw new AssertionError("SystemInfoProcessHandler is null");
        }
        assertThat(handler, is(instanceOf(SystemInfoProcessHandler.class)));
        handler.initialize();

        waitForAssert(() -> {
            final var statusInfo = thing.getStatusInfo();
            assertThat(String.format("Thing status detail is %s with description %s", statusInfo.getStatusDetail(),
                    statusInfo.getDescription()), thing.getStatus(), is(equalTo(ThingStatus.ONLINE)));
        });

        this.thing = thing;

        initializeItem(channelUID, TEST_ITEM_NAME, acceptedItemType);
    }
}
