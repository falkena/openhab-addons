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

import static java.lang.Thread.sleep;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.mockito.Mockito.lenient;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.BINDING_ID;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.BRIDGE_TYPE_COMPUTER;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_LOAD;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.HIGH_PRIORITY_REFRESH_TIME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.LOW_PRIORITY_REFRESH_TIME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.MEDIUM_PRIORITY_REFRESH_TIME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.PRIORITY_PARAMETER;

import java.util.Map;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.Mock;
import org.openhab.binding.systeminfo.internal.SystemInfoHandlerFactory;
import org.openhab.binding.systeminfo.internal.handler.SystemInfoComputerHandler;
import org.openhab.binding.systeminfo.internal.model.OSHISystemInfo;
import org.openhab.binding.systeminfo.internal.model.SystemInfoInterface;
import org.openhab.binding.systeminfo.test.data.SystemInfoMockedCentralProcessor;
import org.openhab.core.config.core.Configuration;
import org.openhab.core.i18n.UnitProvider;
import org.openhab.core.items.Item;
import org.openhab.core.items.ItemNotFoundException;
import org.openhab.core.items.ItemRegistry;
import org.openhab.core.library.items.NumberItem;
import org.openhab.core.library.items.StringItem;
import org.openhab.core.library.types.StringType;
import org.openhab.core.test.java.JavaOSGiTest;
import org.openhab.core.test.storage.VolatileStorageService;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.ManagedThingProvider;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingProvider;
import org.openhab.core.thing.ThingRegistry;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusInfo;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.BridgeHandler;
import org.openhab.core.thing.binding.ThingHandler;
import org.openhab.core.thing.binding.ThingHandlerFactory;
import org.openhab.core.thing.binding.builder.BridgeBuilder;
import org.openhab.core.thing.binding.builder.ChannelBuilder;
import org.openhab.core.thing.link.ItemChannelLink;
import org.openhab.core.thing.link.ManagedItemChannelLinkProvider;
import org.openhab.core.thing.type.ChannelKind;
import org.openhab.core.thing.type.ChannelTypeUID;
import org.openhab.core.types.State;

import oshi.hardware.CentralProcessor;

/**
 * Basis class for OSGi tests
 *
 * @author Svilen Valkanov - Initial contribution
 * @author Lyubomir Papazov - Created a mock systeminfo object. This way, access to the user's OS will not be required,
 *         but mock data will be used instead, avoiding potential errors from the OS queries.
 * @author Wouter Born - Migrate Groovy to Java tests
 * @author Mark Herwege - Processor frequency channels
 */
@NonNullByDefault
public class SystemInfoOSGiTestBase extends JavaOSGiTest {
    private @NonNullByDefault({}) ItemRegistry itemRegistry;
    protected @NonNullByDefault({}) ThingRegistry thingRegistry;

    private @NonNullByDefault({}) ManagedItemChannelLinkProvider itemChannelLinkProvider;
    protected @NonNullByDefault({}) ManagedThingProvider managedThingProvider;
    private @NonNullByDefault({}) UnitProvider unitProvider;

    private @NonNullByDefault({}) SystemInfoHandlerFactory systemInfoHandlerFactory;
    private @Nullable VolatileStorageService storageService;

    protected @Mock @NonNullByDefault({}) OSHISystemInfo mockedSystemInfo;
    protected @Nullable Bridge systemInfoBridge;
    protected @Nullable Item testItem;

    protected static final String DEFAULT_TEST_THING_NAME = "work";
    protected static final String DEFAULT_CHANNEL_TEST_PRIORITY = "High";
    protected static final String DEFAULT_TEST_CHANNEL_ID = CHANNEL_LOAD;
    protected static final int DEFAULT_DEVICE_INDEX = 0;

    /**
     * Refresh time in seconds for tasks with priority High.
     * Default value for the parameter interval_high in the thing configuration
     */
    protected static final int DEFAULT_TEST_INTERVAL_HIGH = 1;

    /**
     * Refresh time in seconds for tasks with priority Medium.
     */
    protected static final int DEFAULT_TEST_INTERVAL_MEDIUM = 3;

    /**
     * Refresh time in seconds for tasks with priority Low.
     */
    protected static final int DEFAULT_TEST_INTERVAL_LOW = 5;

    protected final static Configuration configuration = new Configuration(Map.ofEntries( // Refresh time configuration
            Map.entry(HIGH_PRIORITY_REFRESH_TIME, DEFAULT_TEST_INTERVAL_HIGH), // High priority
            Map.entry(MEDIUM_PRIORITY_REFRESH_TIME, DEFAULT_TEST_INTERVAL_MEDIUM), // Medium priority
            Map.entry(LOW_PRIORITY_REFRESH_TIME, DEFAULT_TEST_INTERVAL_LOW) // Low priority
    ));

    private final CentralProcessor cpu = new SystemInfoMockedCentralProcessor();

    @BeforeEach
    public void initialize() {
        VolatileStorageService storageService = this.storageService;
        if (storageService != null) {
            unregisterService(storageService);
        }
        storageService = new VolatileStorageService();
        registerService(storageService);
        this.storageService = storageService;

        // Preparing the mock with OS properties, that are used in the initialize method of SystemInfoHandler
        lenient().when(mockedSystemInfo.getCPUSpecification()).thenReturn(cpu);
        lenient().when(mockedSystemInfo.getOsFamily()).thenReturn(new StringType("Mock OS"));
        lenient().when(mockedSystemInfo.getOsManufacturer()).thenReturn(new StringType("Mock OS Manufacturer"));
        lenient().when(mockedSystemInfo.getOsVersion()).thenReturn(new StringType("Mock Os Version"));

        // Following mock method returns will make sure the thing does not get recreated with extra channels
        lenient().when(mockedSystemInfo.getDisplayCount()).thenReturn(1);
        lenient().when(mockedSystemInfo.getPowerSourceCount()).thenReturn(1);
        lenient().when(mockedSystemInfo.getFanCount()).thenReturn(1);

        registerService(mockedSystemInfo);

        waitForAssert(() -> {
            final SystemInfoHandlerFactory factory = getService(ThingHandlerFactory.class,
                    SystemInfoHandlerFactory.class);
            assertThat(factory, is(notNullValue()));
            systemInfoHandlerFactory = factory;
        });

        // Unbind oshiSystemInfo service and bind the mock service to make the systeminfo binding tests
        // independent of the external OSHI library
        final SystemInfoInterface oshiSystemInfo = getService(SystemInfoInterface.class);
        if (oshiSystemInfo != null) {
            systemInfoHandlerFactory.unbindSystemInfo(oshiSystemInfo);
        }
        systemInfoHandlerFactory.bindSystemInfo(mockedSystemInfo);

        waitForAssert(() -> {
            final ThingRegistry registry = getService(ThingRegistry.class);
            assertThat(registry, is(notNullValue()));
            thingRegistry = registry;
        });

        waitForAssert(() -> {
            final ItemRegistry registry = getService(ItemRegistry.class);
            assertThat(registry, is(notNullValue()));
            itemRegistry = registry;
        });

        waitForAssert(() -> {
            final ManagedThingProvider provider = getService(ThingProvider.class, ManagedThingProvider.class);
            assertThat(provider, is(notNullValue()));
            managedThingProvider = provider;
        });

        waitForAssert(() -> {
            final ManagedItemChannelLinkProvider provider = getService(ManagedItemChannelLinkProvider.class);
            assertThat(provider, is(notNullValue()));
            itemChannelLinkProvider = provider;
        });

        waitForAssert(() -> {
            final UnitProvider provider = getService(UnitProvider.class);
            assertThat(provider, is(notNullValue()));
            this.unitProvider = provider;
        });
    }

    @AfterEach
    public void dispose() {
        final Bridge bridge = systemInfoBridge;
        systemInfoBridge = null;

        if (bridge != null) {
            // Remove the systeminfo thing. The handler will also be disposed automatically
            final Thing removedThing = thingRegistry.forceRemove(bridge.getUID());
            if (removedThing == null) {
                throw new AssertionError("The computer bridge cannot be deleted");
            }

            managedThingProvider.remove(removedThing.getUID());

            final ThingHandler handler = removedThing.getHandler();
            assertThat(handler, is(nullValue()));
        }

        final Item item = testItem;
        if (item != null) {
            itemRegistry.remove(item.getName());
            testItem = null;
        }
        unregisterService(mockedSystemInfo);

        final VolatileStorageService storageService = this.storageService;
        if (storageService != null) {
            unregisterService(storageService);
            this.storageService = null;
        }
    }

    protected void assertItemState(String itemName, String priority, State expectedState) {
        // The binding starts all refresh tasks in SystemInfoHandler.scheduleUpdates() after this delay !
        try {
            sleep(SystemInfoComputerHandler.WAIT_TIME_CHANNEL_ITEM_LINK_INIT * 1000);
        } catch (InterruptedException e) {
            throw new AssertionError("Interrupted while sleeping");
        }

        final Item item;
        try {
            item = itemRegistry.getItem(itemName);
        } catch (ItemNotFoundException e) {
            throw new AssertionError("Item not found in registry");
        }

        int waitTime;
        if ("High".equals(priority)) {
            waitTime = DEFAULT_TEST_INTERVAL_HIGH * 1000;
        } else if ("Medium".equals(priority)) {
            waitTime = DEFAULT_TEST_INTERVAL_MEDIUM * 1000;
        } else {
            waitTime = 100;
        }

        waitForAssert(() -> {
            final State itemState = item.getState();
            assertThat(itemState, is(equalTo(expectedState)));
        }, waitTime, DFL_SLEEP_TIME);
    }

    protected void initializeThingWithChannel(String channelID, String acceptedItemType) {
        initializeThing(configuration, channelID, acceptedItemType, DEFAULT_CHANNEL_TEST_PRIORITY);
    }

    protected void initializeThing(Configuration thingConfiguration, @Nullable String channelID,
            String acceptedItemType, String priority) {
        final ThingUID thingUID = new ThingUID(BRIDGE_TYPE_COMPUTER, DEFAULT_TEST_THING_NAME);

        final BridgeBuilder builder = BridgeBuilder.create(BRIDGE_TYPE_COMPUTER, thingUID);
        builder.withConfiguration(thingConfiguration);

        if (channelID != null) {
            final ChannelUID channelUID = new ChannelUID(thingUID, channelID);

            ChannelBuilder channelBuilder = ChannelBuilder.create(channelUID, acceptedItemType);
            channelBuilder.withType(new ChannelTypeUID(BINDING_ID, channelUID.getIdWithoutGroup()));
            channelBuilder.withKind(ChannelKind.STATE);

            Configuration channelConfiguration = new Configuration();
            channelConfiguration.put(PRIORITY_PARAMETER, priority);
            channelBuilder.withConfiguration(channelConfiguration);
            builder.withChannel(channelBuilder.build());
        }

        final Bridge bridge = builder.build();
        managedThingProvider.add(bridge);

        final BridgeHandler handler = bridge.getHandler();
        if (handler == null) {
            throw new AssertionError("Bridge handler is null");
        }
        assertThat(handler, is(instanceOf(SystemInfoComputerHandler.class)));
        handler.initialize();

        waitForAssert(() -> {
            final ThingStatusInfo statusInfo = bridge.getStatusInfo();
            assertThat(String.format("Bridge status detail is %s with description %s", statusInfo.getStatusDetail(),
                    statusInfo.getDescription()), bridge.getStatus(), is(equalTo(ThingStatus.ONLINE)));
        });

        systemInfoBridge = bridge;
    }

    protected void initializeItem(ChannelUID channelUID, String itemName, String acceptedItemType) {
        Item item = null;
        if (acceptedItemType.startsWith("Number")) {
            item = new NumberItem(acceptedItemType, itemName, unitProvider);
        } else if ("String".equals(acceptedItemType)) {
            item = new StringItem(itemName);
        }

        if (item == null) {
            throw new AssertionError("Item is null");
        }
        itemRegistry.add(item);
        testItem = item;

        itemChannelLinkProvider.add(new ItemChannelLink(itemName, channelUID));
    }
}
