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
package org.openhab.binding.shelly.internal.discovery;

import static org.openhab.binding.shelly.internal.ShellyBindingConstants.PROPERTY_DEV_GEN;
import static org.openhab.binding.shelly.internal.ShellyBindingConstants.PROPERTY_DEV_TYPE;
import static org.openhab.binding.shelly.internal.ShellyBindingConstants.SHELLY_API_TIMEOUT_MS;
import static org.openhab.binding.shelly.internal.ShellyBindingConstants.SUPPORTED_THING_TYPES_UIDS;
import static org.openhab.binding.shelly.internal.api.ShellyDeviceInfoJsonDTO.SHELLY_DEVICE_INFO_ENDPOINT;
import static org.openhab.binding.shelly.internal.util.ShellyUtils.substringBeforeLast;
import static org.openhab.core.thing.Thing.PROPERTY_MAC_ADDRESS;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.eclipse.jetty.client.HttpClient;
import org.eclipse.jetty.client.api.ContentResponse;
import org.eclipse.jetty.client.api.Request;
import org.eclipse.jetty.http.HttpMethod;
import org.openhab.binding.shelly.internal.api.ShellyDeviceInfoJsonDTO;
import org.openhab.binding.shelly.internal.api1.Shelly1ApiJsonDTO;
import org.openhab.binding.shelly.internal.config.ShellyBindingConfiguration;
import org.openhab.binding.shelly.internal.config.ShellyThingConfiguration;
import org.openhab.binding.shelly.internal.handler.ShellyThingTable;
import org.openhab.core.config.discovery.AbstractDiscoveryService;
import org.openhab.core.config.discovery.DiscoveryResult;
import org.openhab.core.config.discovery.DiscoveryResultBuilder;
import org.openhab.core.config.discovery.DiscoveryService;
import org.openhab.core.thing.ThingUID;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;

/**
 * Device discovery creates a thing in the inbox for each vehicle
 * found in the data received from {@link ShellyBasicDiscoveryService}.
 *
 * @author Markus Michels - Initial Contribution
 *
 */
@NonNullByDefault
public class ShellyBasicDiscoveryService extends AbstractDiscoveryService {
    private final Logger logger = LoggerFactory.getLogger(ShellyBasicDiscoveryService.class);

    private final BundleContext bundleContext;
    private final ShellyThingTable thingTable;
    private @Nullable ServiceRegistration<?> discoveryService;

    private static final int TIMEOUT = 10;
    private static final Gson gson = new Gson();

    public ShellyBasicDiscoveryService(BundleContext bundleContext, ShellyThingTable thingTable) {
        super(SUPPORTED_THING_TYPES_UIDS, TIMEOUT);
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

    public void discoveredResult(String model, String serviceName, String address, Map<String, Object> properties) {
        ThingUID uid = ShellyThingCreator.getThingUID(serviceName, model, "", true);
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
        Logger logger = LoggerFactory.getLogger(ShellyBasicDiscoveryService.class);

        final ContentResponse response;
        try {
            final URI uri = new URI("http", ipAddress, SHELLY_DEVICE_INFO_ENDPOINT, null);
            final Request request = httpClient.newRequest(uri).method(HttpMethod.GET);
            response = request.timeout(SHELLY_API_TIMEOUT_MS, TimeUnit.MILLISECONDS).send();
        } catch (URISyntaxException | ExecutionException | InterruptedException | TimeoutException exception) {
            logger.debug("Discovery: Unable to discover thing", exception);
            return null;
        }

        final String json = response.getContentAsString().trim();
        // Need??? .replace("\t", "").replace("\r\n", "").trim()
        final ShellyDeviceInfoJsonDTO content = gson.fromJson(json, ShellyDeviceInfoJsonDTO.class);
        if (content == null) {
            logger.debug("Discovery: Unable to discover thing");
            return null;
        }

        final Shelly1ApiJsonDTO.ShellySettingsDevice settings = new Shelly1ApiJsonDTO.ShellySettingsDevice();
        settings.auth = content.needAuthentication();
        settings.mac = content.getDeviceMacAddress();
        settings.type = content.getDeviceModel();
        settings.fw = content.getFirmwareVersion();
        settings.gen = content.getDeviceGeneration();

        switch (content.getApiVersion()) {
            case ShellyDeviceInfoJsonDTO.ApiVersion.V1: {
                // settings.mode; // Gen 1
                // settings.id; // Gen2: service name
                // settings.name; // Gen2: configured device name
                // settings.profile; // Gen 2
                settings.hostname = hostname;
                // settings.coiot;
                break;
            }
            case ShellyDeviceInfoJsonDTO.ApiVersion.V2: {
                settings.hostname = content.getDeviceId();
                // settings.name = getString(device.name);
                // settings.mode = mapValue(MAP_PROFILE, device.profile);
                // Not Boolean, but String settings.coiot = false;
                break;
            }
            default: {
                logger.info("Discovery: Received not supported API version");
                return null;
            }
        }
        // get thing type from device name
        final ThingUID thingUID = ShellyThingCreator.getThingUID(settings.hostname, settings.type, settings.mode,
                false);

        final Map<String, Object> properties = new HashMap<>(); // ShellyBaseHandler.fillDeviceProperties(profile);

        addProperty(properties, PROPERTY_MAC_ADDRESS, content.getDeviceMacAddress());
        // addProperty(properties, CONFIG_DEVICEIP, ipAddress);
        // addProperty(properties, PROPERTY_MODEL_ID, model);
        // addProperty(properties, PROPERTY_SERVICE_NAME, name);
        // addProperty(properties, PROPERTY_DEV_NAME, deviceName);
        addProperty(properties, PROPERTY_DEV_TYPE, substringBeforeLast(settings.hostname, "-"));
        addProperty(properties, PROPERTY_DEV_GEN, content.getDeviceGeneration().toString());
        // addProperty(properties, PROPERTY_DEV_MODE, mode);
        // addProperty(properties, PROPERTY_DEV_AUTH, auth ? "yes" : "no");

        final DiscoveryResultBuilder builder = DiscoveryResultBuilder.create(thingUID);
        builder.withLabel(String.format("%s - %s", hostname, ipAddress)).withProperties(properties);
        return builder.withRepresentationProperty(PROPERTY_MAC_ADDRESS).build();

        /*
         * ThingUID thingUID = null;
         * ShellySettingsDevice devInfo;
         * ShellyApiInterface api = null;
         * boolean auth = false;
         * String mac = "";
         * String model = "";
         * String mode = "";
         * String name = hostname;
         * String deviceName = "";
         * String thingType = "";
         * Map<String, Object> properties = new TreeMap<>();
         * 
         * try {
         * ShellyThingConfiguration config = fillConfig(bindingConfig, ipAddress);
         * api = gen2 ? new Shelly2ApiRpc(name, config, httpClient) : new Shelly1HttpApi(name, config, httpClient);
         * api.initialize();
         * devInfo = api.getDeviceInfo();
         * mac = getString(devInfo.mac);
         * model = devInfo.type;
         * auth = getBool(devInfo.auth);
         * if (name.isEmpty() || name.startsWith("shellyplusrange")) {
         * name = devInfo.hostname;
         * }
         * if (devInfo.name != null) {
         * deviceName = devInfo.name;
         * }
         * 
         * thingType = substringBeforeLast(name, "-");
         * final ShellyDeviceProfile profile = api.getDeviceProfile(thingType, devInfo);
         * deviceName = profile.name;
         * mode = devInfo.mode;
         * properties = ShellyBaseHandler.fillDeviceProperties(profile);
         * 
         * // get thing type from device name
         * thingUID = ShellyThingCreator.getThingUID(name, model, mode, false);
         * } catch (ShellyApiException exception) {
         * final ShellyApiResult result = exception.getApiResult();
         * if (result.isHttpAccessUnauthorized()) {
         * // create shellyunknown thing - will be changed during thing initialization with valid credentials
         * thingUID = ShellyThingCreator.getThingUID(name, model, mode, true);
         * }
         * } catch (IllegalArgumentException | IOException exception) { // maybe some format description was buggy
         * logger.debug("Discovery: Unable to discover thing", exception);
         * } finally {
         * if (api != null) {
         * api.close();
         * }
         * }
         * 
         * if (thingUID != null) {
         * addProperty(properties, PROPERTY_MAC_ADDRESS, mac);
         * addProperty(properties, CONFIG_DEVICEIP, ipAddress);
         * addProperty(properties, PROPERTY_MODEL_ID, model);
         * addProperty(properties, PROPERTY_SERVICE_NAME, name);
         * addProperty(properties, PROPERTY_DEV_NAME, deviceName);
         * addProperty(properties, PROPERTY_DEV_TYPE, thingType);
         * addProperty(properties, PROPERTY_DEV_GEN, gen2 ? "2" : "1");
         * addProperty(properties, PROPERTY_DEV_MODE, mode);
         * addProperty(properties, PROPERTY_DEV_AUTH, auth ? "yes" : "no");
         * 
         * String thingLabel = deviceName.isEmpty() ? name + " - " + ipAddress
         * : deviceName + " (" + name + "@" + ipAddress + ")";
         * return DiscoveryResultBuilder.create(thingUID).withProperties(properties).withLabel(thingLabel)
         * .withRepresentationProperty(PROPERTY_MAC_ADDRESS).build();
         * }
         * 
         * return null;
         */
    }

    public static ShellyThingConfiguration fillConfig(ShellyBindingConfiguration bindingConfig, String address)
            throws IOException {
        ShellyThingConfiguration config = new ShellyThingConfiguration();
        config.deviceIp = address;
        config.userId = bindingConfig.defaultUserId;
        config.password = bindingConfig.defaultPassword;
        return config;
    }

    private static void addProperty(Map<String, Object> properties, String key, @Nullable String value) {
        properties.put(key, value != null ? value : "");
    }
}
