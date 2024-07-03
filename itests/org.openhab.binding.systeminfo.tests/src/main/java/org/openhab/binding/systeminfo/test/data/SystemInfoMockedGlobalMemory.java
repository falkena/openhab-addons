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

import java.util.List;

import org.eclipse.jdt.annotation.NonNullByDefault;

import oshi.hardware.GlobalMemory;
import oshi.hardware.PhysicalMemory;
import oshi.hardware.VirtualMemory;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoMockedGlobalMemory implements GlobalMemory {
    private final VirtualMemory virtualMemory = new SystemInfoMockedVirtualMemory();
    private final PhysicalMemory physicalMemory = new PhysicalMemory(TEST_MEMORY_BANK_LABEL, TEST_MEMORY_BANK_CAPACITY,
            TEST_MEMORY_BANK_CLOCK_SPEED, TEST_DRIVE_MANUFACTURER, TEST_MEMORY_BANK_TYPE, TEST_MEMORY_PART_NUMBER,
            TEST_MEMORY_SERIAL_NUMBER);

    public static final long TEST_MEMORY_BANK_CAPACITY = 1024;
    public static final long TEST_MEMORY_BANK_CLOCK_SPEED = 10;
    public static final String TEST_MEMORY_BANK_LABEL = "Mocked Bank Label";
    public static final String TEST_DRIVE_MANUFACTURER = "Mocked Memory Manufacturer";
    public static final String TEST_MEMORY_BANK_TYPE = "Mocked Memory Type";
    public static final String TEST_MEMORY_PART_NUMBER = "Mocked Memory Part";
    public static final String TEST_MEMORY_SERIAL_NUMBER = "Mocked Memory Serial";

    public static final long TEST_MEMORY_AVAILABLE = 1000;
    public static final long TEST_MEMORY_TOTAL = 2048;
    public static final long TEST_MEMORY_USED = TEST_MEMORY_TOTAL - TEST_MEMORY_AVAILABLE;
    public static final long TEST_MEMORY_PAGE_SIZE = 1024;

    @Override
    public long getPageSize() {
        return TEST_MEMORY_PAGE_SIZE;
    }

    @Override
    public long getTotal() {
        return TEST_MEMORY_TOTAL;
    }

    @Override
    public long getAvailable() {
        return TEST_MEMORY_AVAILABLE;
    }

    @Override
    public VirtualMemory getVirtualMemory() {
        return virtualMemory;
    }

    @Override
    @NonNullByDefault({})
    public List<PhysicalMemory> getPhysicalMemory() {
        return List.of(physicalMemory);
    }
}
