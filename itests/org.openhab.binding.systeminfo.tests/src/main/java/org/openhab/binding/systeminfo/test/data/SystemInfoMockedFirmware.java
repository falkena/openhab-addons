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
package org.openhab.binding.systeminfo.test.data;

import org.eclipse.jdt.annotation.NonNullByDefault;

import oshi.hardware.Firmware;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoMockedFirmware implements Firmware {
    public static final String TEST_FIRMWARE_DESCRIPTION = "Mocked Firmware Description";
    public static final String TEST_FIRMWARE_NAME = "Mocked Firmware Name";
    public static final String TEST_FIRMWARE_MANUFACTURER = "Mocked Firmware Manufacturer";
    public static final String TEST_FIRMWARE_RELEASE_DATE = "Mocked Firmware Release Date";
    public static final String TEST_FIRMWARE_VERSION = "Mocked Firmware Version";

    @Override
    public String getManufacturer() {
        return TEST_FIRMWARE_MANUFACTURER;
    }

    @Override
    public String getName() {
        return TEST_FIRMWARE_NAME;
    }

    @Override
    public String getDescription() {
        return TEST_FIRMWARE_DESCRIPTION;
    }

    @Override
    public String getVersion() {
        return TEST_FIRMWARE_VERSION;
    }

    @Override
    public String getReleaseDate() {
        return TEST_FIRMWARE_RELEASE_DATE;
    }
}
