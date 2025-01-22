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
package org.openhab.binding.shelly.internal.api;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;

/**
 * {@link ShellyDeviceInfoJsonDTO} wraps the Shelly REST API and provides various low level function to access
 * the device api (not cloud api).
 *
 * See https://shelly-api-docs.shelly.cloud/gen2/ComponentsAndServices/Shelly#http-endpoint-shelly and
 * https://shelly-api-docs.shelly.cloud/gen1/#http-dialect for details
 *
 * @author Markus Michels - Initial contribution
 */
@NonNullByDefault
public class ShellyDeviceInfoJsonDTO {
    public enum ApiVersion {
        V1,
        V2
    }

    public static final String INVALID = "INVALID";
    public static final String SHELLY_DEVICE_INFO_ENDPOINT = "/shelly";

    private @Nullable String type = null; // Model of the device. Is set for on Gen1 devices.
    private String mac = INVALID; // MAC address of the device. Is set for Gen1 and higher devices.
    private @Nullable Boolean auth = null; // Whether authentication is required. Is set on Gen1 devices.
    private @Nullable String fw = null; // Current firmware version. Is set on Gen1 devices.
    // private Number longid 1 if the device identifies itself with its full MAC address; 0 if only the last 3 bytes are
    // used

    private @Nullable String id = null; // Id of the device. Is set on Gen2+ devices.
    private @Nullable String model = null; // Model of the device. Is set on Gen2+ devices.
    private @Nullable Integer gen = null; // Generation of the device. Is set on Gen2+ devices.
    private @Nullable String fw_id = null; // Id of the firmware. Is set on Gen2+ devices.
    private @Nullable String ver = null; // Version of the firmware. Is set on Gen2+ devices.
    // private String app Application name
    // private String profile Name of the device profile (only applicable for multi-profile devices)
    private @Nullable Boolean auth_en = null; // Whether authentication is required. Is set on Gen2+ devices.
    // private @Nullable String auth_domain Name of the domain (null if authentication is not enabled)
    // private Boolean discoverable Present only when false. If true, device is shown in 'Discovered devices'. If false,
    // the device is hidden.
    // private String key Cloud key of the device (see note below), present only when the ident parameter is set to true
    // private String batch Batch used to provision the device, present only when the ident parameter is set to true
    // private String fw_sbits Shelly internal flags, present only when the ident parameter is set to true

    public ApiVersion getApiVersion() {
        return (getDeviceGeneration() == 1) ? ApiVersion.V1 : ApiVersion.V2;
    }

    // Partially duplicated with ShellyDeviceProfile::extractFwVersion
    public String getFirmwareVersion() {
        final String version;
        if (getApiVersion() == ApiVersion.V1) {
            version = this.fw; // Try to extract Gen1 devices firmware version
        } else {
            final String firmware = this.ver;
            if (firmware != null) {
                return firmware.trim(); // Return Gen2 or higher firmware version, since provided
            } else {
                version = this.fw_id; // Try to extract Gen2 or higher firmware version
            }
        }

        // fix version e.g.
        if (version != null) {
            // ToDo: find more general regex instead of string replacing
            // 20210319-122304/v.1.10-Dimmer1-gfd4cc10 (with v.1. instead of v1.)
            String firmware = version.replace("/v.1.10-", "/v1.10.0-");

            // 20220809-125346/v1.12-g99f7e0b (.0 in 1.12.0 missing)
            firmware = firmware.replace("/v1.12-", "/v1.12.0");

            final String regex;
            if (getApiVersion() == ApiVersion.V1) {
                regex = "v\\d+\\.\\d+\\.\\d+(-[a-z0-9]*)?";
            } else {
                regex = "\\d+\\.\\d+\\.\\d+(-[a-fh-z0-9]*)?";
            }

            final Matcher matcher = Pattern.compile(regex).matcher(firmware);
            return matcher.find() ? matcher.group(0).trim() : INVALID;
        } else {
            return INVALID;
        }
    }

    public String getDeviceId() {
        if (getApiVersion() == ApiVersion.V1) {
            return INVALID; // Construct it: <WhatEver>-mac.toLower() or it can be found in /settings#hostname endpoint
        } else {
            final String id = this.id;
            return (id != null) ? id.trim() : INVALID;
        }
    }

    public Integer getDeviceGeneration() {
        final Integer generation = this.gen;
        return (generation != null) ? generation : 1;
    }

    public String getDeviceMacAddress() {
        final String mac = this.mac.toUpperCase().trim();
        return mac.replaceAll("(.{2})(?!$)", "$1:");
    }

    public String getDeviceModel() {
        final String model = (getApiVersion() == ApiVersion.V1) ? this.type : this.model;
        return (model != null) ? model.trim() : INVALID;
    }

    public Boolean needAuthentication() {
        final Boolean authentication = (getApiVersion() == ApiVersion.V1) ? this.auth : this.auth_en;
        return (authentication != null) ? authentication : false;
    }
}
