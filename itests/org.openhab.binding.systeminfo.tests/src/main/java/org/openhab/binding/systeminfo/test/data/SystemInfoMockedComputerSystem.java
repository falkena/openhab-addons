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

import oshi.hardware.Baseboard;
import oshi.hardware.ComputerSystem;
import oshi.hardware.Firmware;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoMockedComputerSystem implements ComputerSystem {
    public static final String TEST_SYSTEM_HARDWARE_UUID = "Mocked Hardware UUID";
    public static final String TEST_SYSTEM_MANUFACTURER = "Mocked System Manufacturer";
    public static final String TEST_SYSTEM_MODEL = "Mocked System Model";
    public static final String TEST_SYSTEM_SERIAL = "Mocked System Serial";

    private final Baseboard board = new SystemInfoMockedBaseboard();
    private final Firmware firmware = new SystemInfoMockedFirmware();

    @Override
    public String getManufacturer() {
        return TEST_SYSTEM_MANUFACTURER;
    }

    @Override
    public String getModel() {
        return TEST_SYSTEM_MODEL;
    }

    @Override
    public String getSerialNumber() {
        return TEST_SYSTEM_SERIAL;
    }

    @Override
    public String getHardwareUUID() {
        return TEST_SYSTEM_HARDWARE_UUID;
    }

    @Override
    public Firmware getFirmware() {
        return firmware;
    }

    @Override
    public Baseboard getBaseboard() {
        return board;
    }
}
