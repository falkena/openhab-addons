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
package org.openhab.binding.systeminfo.test.discovery;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.when;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.BRIDGE_TYPE_COMPUTER;
import static org.openhab.binding.systeminfo.internal.discovery.SystemInfoComputerDiscoveryService.DEFAULT_THING_ID;
import static org.openhab.binding.systeminfo.internal.discovery.SystemInfoComputerDiscoveryService.DEFAULT_THING_LABEL;
import static org.openhab.core.thing.ThingStatus.ONLINE;

import java.net.UnknownHostException;
import java.util.List;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.openhab.binding.systeminfo.internal.SystemInfoHandlerFactory;
import org.openhab.binding.systeminfo.internal.discovery.SystemInfoComputerDiscoveryService;
import org.openhab.binding.systeminfo.internal.model.OSHISystemInfo;
import org.openhab.binding.systeminfo.internal.model.SystemInfoInterface;
import org.openhab.core.config.discovery.DiscoveryResult;
import org.openhab.core.config.discovery.DiscoveryService;
import org.openhab.core.config.discovery.inbox.Inbox;
import org.openhab.core.config.discovery.inbox.InboxPredicates;
import org.openhab.core.library.types.DecimalType;
import org.openhab.core.library.types.StringType;
import org.openhab.core.test.java.JavaOSGiTest;
import org.openhab.core.test.storage.VolatileStorageService;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingRegistry;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.ThingHandlerFactory;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class SystemInfoComputerDiscoveryOSGiTest extends JavaOSGiTest {

    private @Mock @NonNullByDefault({}) OSHISystemInfo mockedSystemInfo;

    private @Nullable ThingRegistry thingRegistry;
    private @Nullable VolatileStorageService storageService;

    static class SystemInfoDiscoveryServiceMock extends SystemInfoComputerDiscoveryService {
        String hostname;

        SystemInfoDiscoveryServiceMock(String hostname) {
            super();
            this.hostname = hostname;
        }

        @Override
        protected String getHostName() throws UnknownHostException {
            if ("unresolved".equals(hostname)) {
                throw new UnknownHostException();
            }
            return hostname;
        }

        @Override
        public void startScan() {
            super.startScan();
        }
    }

    @BeforeEach
    public void setUp() {
        VolatileStorageService storageService = this.storageService;
        if (storageService != null) {
            unregisterService(storageService);
        }
        storageService = new VolatileStorageService();
        registerService(storageService);
        this.storageService = storageService;

        // Fill required properties, that thing can go online
        when(mockedSystemInfo.getCpuLogicalCores()).thenReturn(new DecimalType(1));
        when(mockedSystemInfo.getCpuPhysicalCores()).thenReturn(new DecimalType(1));
        when(mockedSystemInfo.getOsFamily()).thenReturn(new StringType("Mock OS"));
        when(mockedSystemInfo.getOsManufacturer()).thenReturn(new StringType("Mock OS Manufacturer"));
        when(mockedSystemInfo.getOsVersion()).thenReturn(new StringType("Mock OS Version"));

        // Populate values for channel configuration, that thing can go online
        when(mockedSystemInfo.getFileOSStoreCount()).thenReturn(1);
        when(mockedSystemInfo.getDisplayCount()).thenReturn(1);
        when(mockedSystemInfo.getPowerSourceCount()).thenReturn(1);

        registerService(mockedSystemInfo);

        waitForAssert(() -> {
            final SystemInfoHandlerFactory factory = getService(ThingHandlerFactory.class,
                    SystemInfoHandlerFactory.class);
            if (factory == null) {
                throw new AssertionError("Thing handler factory is null");
            }

            final SystemInfoInterface systemInfo = getService(SystemInfoInterface.class);
            if (systemInfo != null) {
                factory.unbindSystemInfo(systemInfo);
            }
            factory.bindSystemInfo(mockedSystemInfo);
        });

        final ThingRegistry registry = getService(ThingRegistry.class);
        waitForAssert(() -> assertThat(registry, is(notNullValue())));
        thingRegistry = registry;
    }

    @AfterEach
    public void tearDown() {
        thingRegistry = null;

        unregisterService(mockedSystemInfo);

        final VolatileStorageService storageService = this.storageService;
        if (storageService != null) {
            unregisterService(storageService);
            this.storageService = null;
        }
    }

    @Test
    public void testDiscoveryWithInvalidHostname() {
        testDiscoveryService("Hilo_fritz_box", "Hilo.fritz.box");
    }

    @Test
    public void testDiscoveryWithValidHostname() {
        testDiscoveryService("MyComputer", "MyComputer");
    }

    @Test
    public void testDiscoveryWithUnresolvedHostname() {
        testDiscoveryService(DEFAULT_THING_ID, "unresolved");
    }

    @Test
    public void testDiscoveryWithEmptyHostnameString() {
        testDiscoveryService(DEFAULT_THING_ID, "");
    }

    private void testDiscoveryService(String expectedHostname, String hostname) {
        if (getService(DiscoveryService.class, SystemInfoComputerDiscoveryService.class) != null) {
            unregisterService(DiscoveryService.class);
        }

        SystemInfoDiscoveryServiceMock discoveryServiceMock = new SystemInfoDiscoveryServiceMock(hostname);
        registerService(discoveryServiceMock, DiscoveryService.class.getName());
        discoveryServiceMock.startScan();

        final ThingUID computer = new ThingUID(BRIDGE_TYPE_COMPUTER, expectedHostname);
        waitForAssert(() -> {
            final Inbox inbox = getService(Inbox.class);
            if (inbox == null) {
                throw new AssertionError("Inbox is null");
            }

            List<DiscoveryResult> results = inbox.stream().filter(InboxPredicates.forThingUID(computer)).toList();
            assertFalse(results.isEmpty(), "No Thing with UID " + computer.getAsString() + " in inbox");
            inbox.approve(computer, DEFAULT_THING_LABEL, null);
        });

        final ThingRegistry thingRegistry = this.thingRegistry;
        if (thingRegistry == null) {
            throw new AssertionError("Thing registry is null");
        }

        final Thing thing = thingRegistry.get(computer);
        if (thing == null) {
            throw new AssertionError("Thing is null");
        }
        assertThat("Thing is not initialized.", thing.getStatus(), is(equalTo(ONLINE)));

        unregisterService(DiscoveryService.class.getName());
    }
}
