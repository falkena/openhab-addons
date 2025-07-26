/*
 * Copyright (c) 2010-2026 Contributors to the openHAB project
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
package org.openhab.binding.shelly.internal.discovery;

import static org.openhab.binding.shelly.internal.ShellyBindingConstants.*;
import static org.openhab.binding.shelly.internal.ShellyDevices.*;
import static org.openhab.binding.shelly.internal.util.ShellyUtils.*;
import static org.openhab.core.thing.Thing.*;

import java.util.Hashtable;
import java.util.Map;
import java.util.TreeMap;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.eclipse.jetty.client.HttpClient;
import org.openhab.binding.shelly.internal.api.ShellyApiException;
import org.openhab.binding.shelly.internal.api.ShellyApiInterface;
import org.openhab.binding.shelly.internal.api.ShellyApiResult;
import org.openhab.binding.shelly.internal.api1.Shelly1ApiJsonDTO.ShellySettingsDevice;
import org.openhab.binding.shelly.internal.api1.Shelly1HttpApi;
import org.openhab.binding.shelly.internal.api2.Shelly2ApiRpc;
import org.openhab.binding.shelly.internal.config.ShellyBindingConfiguration;
import org.openhab.binding.shelly.internal.config.ShellyThingConfiguration;
import org.openhab.binding.shelly.internal.handler.ShellyThingTable;
import org.openhab.core.config.discovery.AbstractDiscoveryService;
import org.openhab.core.config.discovery.DiscoveryResult;
import org.openhab.core.config.discovery.DiscoveryResultBuilder;
import org.openhab.core.config.discovery.DiscoveryService;
import org.openhab.core.thing.ThingTypeUID;
import org.openhab.core.thing.ThingUID;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Device discovery creates a thing in the inbox for each vehicle
 * found in the data received from {@link ShellyBasicDiscoveryService}.
 *
 * @author Markus Michels - Initial Contribution
 *
 */
@NonNullByDefault
public class ShellyBasicDiscoveryService extends AbstractDiscoveryService {
    private static final Logger logger = LoggerFactory.getLogger(ShellyBasicDiscoveryService.class);

    private final BundleContext bundleContext;
    private final ShellyThingTable thingTable;
    private static final int TIMEOUT = 10;
    private @Nullable ServiceRegistration<?> discoveryService;

    public ShellyBasicDiscoveryService(BundleContext bundleContext, ShellyThingTable thingTable) {
        super(SUPPORTED_THING_TYPES, TIMEOUT);
        this.bundleContext = bundleContext;
        this.thingTable = thingTable;
    }

    public void registerDeviceDiscoveryService() {
        if (discoveryService == null) {
            discoveryService = bundleContext.registerService(DiscoveryService.class.getName(), this, new Hashtable<>());
        }
    }

    @Override
    protected void startScan() {
        logger.debug("Starting BLU Discovery");
        thingTable.startScan();
    }

    public void discoveredResult(ThingTypeUID tuid, String model, String serviceName, String address,
            Map<String, Object> properties) {
        ThingUID uid = ShellyThingCreator.getThingUID(serviceName, model, "");
        logger.debug("Adding discovered thing with id {}", uid.toString());
        properties.put(PROPERTY_MAC_ADDRESS, address);
        String thingLabel = "Shelly BLU " + model + " (" + serviceName + ")";
        DiscoveryResult result = DiscoveryResultBuilder.create(uid).withProperties(properties)
                .withRepresentationProperty(PROPERTY_MAC_ADDRESS).withLabel(thingLabel).build();
        thingDiscovered(result);
    }

    public void discoveredResult(DiscoveryResult result) {
        thingDiscovered(result);
    }

    public void unregisterDeviceDiscoveryService() {
        ServiceRegistration<?> discoveryService = this.discoveryService;
        if (discoveryService != null) {
            discoveryService.unregister();
        }
    }

    @Override
    public void deactivate() {
        super.deactivate();
        unregisterDeviceDiscoveryService();
    }

    public static @Nullable DiscoveryResult createResult(String hostname, String ipAddress,
            ShellyBindingConfiguration bindingConfig, HttpClient httpClient) {
        final var config = new ShellyThingConfiguration();
        config.deviceIp = ipAddress;
        config.userId = bindingConfig.defaultUserId;
        config.password = bindingConfig.defaultPassword;

        String name = hostname;

        ThingUID thingUID = null;
        ShellySettingsDevice devInfo;
        ShellyApiInterface api = new Shelly1HttpApi(hostname, config, httpClient);
        try {
            devInfo = api.getDeviceInfo();
            if ((devInfo.gen != null) && (devInfo.gen > 1)) {
                api.close();
                api = new Shelly2ApiRpc(hostname, config, httpClient);
                devInfo = api.getDeviceInfo();
                if (name.isEmpty() || name.startsWith(SERVICE_NAME_SHELLYPLUSRANGE_PREFIX)) {
                    name = devInfo.hostname;
                }
            }
        } catch (ShellyApiException exception) {
            ShellyApiResult result = exception.getApiResult();
            if (result.isHttpAccessUnauthorized()) {
                // create shellyunknown thing - will be changed during thing initialization with valid credentials
                thingUID = ShellyThingCreator.getThingUIDForUnknown(hostname, "", "");
            } else {
                logger.debug("{}: Unable to discover device: {}", name, exception.getMessage());
            }
            devInfo = null;
        } finally {
            api.close();
        }

        Map<String, Object> properties = new TreeMap<>();
        if ((devInfo != null) && (thingUID == null)) {
            final var mode = getString(devInfo.mode);
            final var model = getString(devInfo.type);

            addProperty(properties, CONFIG_DEVICEIP, ipAddress);
            addProperty(properties, PROPERTY_MAC_ADDRESS, getString(devInfo.mac));
            addProperty(properties, PROPERTY_MODEL_ID, model);
            addProperty(properties, PROPERTY_DEV_MODE, mode);
            addProperty(properties, PROPERTY_SERVICE_NAME, name);

            // get thing type from device name
            thingUID = ShellyThingCreator.getThingUID(name, model, mode);
        }

        if (thingUID != null) {
            final var service = properties.getOrDefault(PROPERTY_SERVICE_NAME, "Unknown").toString();

            final var type = substringBeforeLast(service, "-");
            final var mode = properties.getOrDefault(PROPERTY_DEV_MODE, "Unknown").toString();
            final var model = properties.getOrDefault(PROPERTY_MODEL_ID, "Unknown").toString();
            logger.debug("{}: Adding Thing to Inbox (type {}, model {}, mode={})", service, type, model, mode);

            final var builder = DiscoveryResultBuilder.create(thingUID);
            builder.withProperties(properties).withLabel(service + "@" + ipAddress);
            builder.withRepresentationProperty(PROPERTY_MAC_ADDRESS).build();
            return builder.build();
        }

        return null;
    }

    private static void addProperty(Map<String, Object> properties, String key, @Nullable String value) {
        properties.put(key, value != null ? value : "");
    }
}
