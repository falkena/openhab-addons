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
package org.openhab.binding.shelly.internal.handler;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.shelly.internal.discovery.ShellyBasicDiscoveryService;
import org.openhab.core.config.discovery.DiscoveryResult;
import org.osgi.framework.BundleContext;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.ConfigurationPolicy;
import org.osgi.service.component.annotations.Deactivate;

/***
 * The{@link ShellyThingTable} implements a simple table to allow dispatching incoming events to the proper thing
 * handler
 *
 * @author Markus Michels - Initial contribution
 */
@NonNullByDefault
@Component(service = ShellyThingTable.class, configurationPolicy = ConfigurationPolicy.OPTIONAL)
public class ShellyThingTable {
    private final Map<String, ShellyThingInterface> thingTable = new ConcurrentHashMap<>();
    private @Nullable ShellyBasicDiscoveryService discoveryService;

    public void addThing(String key, ShellyThingInterface thing) {
        if (thingTable.containsKey(key)) {
            thingTable.remove(key);
        }
        thingTable.put(key, thing);
    }

    public @Nullable ShellyThingInterface findThing(String key) {
        final ShellyThingInterface thing = thingTable.get(key);
        if (thing != null) {
            return thing;
        }

        for (final ShellyThingInterface entry : thingTable.values()) {
            if (entry.checkRepresentation(key)) {
                return entry;
            }
        }

        return null;
    }

    public ShellyThingInterface getThing(String key) {
        ShellyThingInterface thing = findThing(key);
        if (thing == null) {
            throw new IllegalArgumentException();
        }
        return thing;
    }

    public void removeThing(String key) {
        if (thingTable.containsKey(key)) {
            thingTable.remove(key);
        }
    }

    public Map<String, ShellyThingInterface> getTable() {
        return thingTable;
    }

    public int size() {
        return thingTable.size();
    }

    public void startDiscoveryService(BundleContext bundleContext) {
        ShellyBasicDiscoveryService discoveryService = this.discoveryService;
        if (discoveryService == null) {
            discoveryService = new ShellyBasicDiscoveryService(bundleContext, this);
            discoveryService.registerDeviceDiscoveryService();
            this.discoveryService = discoveryService;
        }
    }

    public void startScan() {
        for (final ShellyThingInterface thing : thingTable.values()) {
            thing.startScan();
        }
    }

    public void stopDiscoveryService() {
        final ShellyBasicDiscoveryService discoveryService = this.discoveryService;
        if (discoveryService != null) {
            discoveryService.unregisterDeviceDiscoveryService();
            this.discoveryService = null;
        }
    }

    public void discoveredResult(String model, String serviceName, String address, Map<String, Object> properties) {
        final ShellyBasicDiscoveryService discoveryService = this.discoveryService;
        if (discoveryService != null) {
            discoveryService.discoveredResult(model, serviceName, address, properties);
        }
    }

    public void discoveredResult(DiscoveryResult result) {
        final ShellyBasicDiscoveryService discoveryService = this.discoveryService;
        if (discoveryService != null) {
            discoveryService.discoveredResult(result);
        }
    }

    @Deactivate
    public void deactivate() {
        stopDiscoveryService();
    }
}
