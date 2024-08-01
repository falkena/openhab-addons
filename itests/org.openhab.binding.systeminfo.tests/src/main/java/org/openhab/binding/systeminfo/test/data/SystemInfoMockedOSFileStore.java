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

import static org.openhab.binding.systeminfo.test.data.SystemInfoMockedHWDiskStore.TEST_PARTITION_MOUNT;
import static org.openhab.binding.systeminfo.test.data.SystemInfoMockedHWDiskStore.TEST_PARTITION_SIZE;
import static org.openhab.binding.systeminfo.test.data.SystemInfoMockedHWDiskStore.TEST_PARTITION_UUID;

import org.eclipse.jdt.annotation.NonNullByDefault;

import oshi.software.os.OSFileStore;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoMockedOSFileStore implements OSFileStore {

    public static final String TEST_STORE_DESCRIPTION = "Mocked Store Description";
    public static final String TEST_STORE_LABEL = "Mocked Store Label";
    public static final String TEST_STORE_LOGICAL_VOLUME = "Mocked Store Logical Volume";
    public static final String TEST_STORE_MOUNT = TEST_PARTITION_MOUNT;
    public static final String TEST_STORE_NAME = "Mocked Store Name";
    public static final String TEST_STORE_OPTIONS = "Mocked Store Options";
    public static final String TEST_STORE_TYPE = "Mocked Store Type";
    public static final String TEST_STORE_UUID = TEST_PARTITION_UUID;
    public static final String TEST_STORE_VOLUME = "Mocked Store Volume";

    public static final long TEST_STORE_TOTAL_SPACE = TEST_PARTITION_SIZE;
    public static final long TEST_STORE_FREE_SPACE = TEST_STORE_TOTAL_SPACE / 2;
    public static final long TEST_STORE_USABLE_SPACE = 3 * TEST_STORE_FREE_SPACE / 4;
    public static final long TEST_STORE_TOTAL_INODES = 1024;
    public static final long TEST_STORE_FREE_INODES = TEST_STORE_TOTAL_INODES / 2;

    @Override
    public String getDescription() {
        return TEST_STORE_DESCRIPTION;
    }

    @Override
    public String getLabel() {
        return TEST_STORE_LABEL;
    }

    @Override
    public String getLogicalVolume() {
        return TEST_STORE_LOGICAL_VOLUME;
    }

    @Override
    public String getMount() {
        return TEST_STORE_MOUNT;
    }

    @Override
    public String getName() {
        return TEST_STORE_NAME;
    }

    @Override
    public String getOptions() {
        return TEST_STORE_OPTIONS;
    }

    @Override
    public String getType() {
        return TEST_STORE_TYPE;
    }

    @Override
    public String getVolume() {
        return TEST_STORE_VOLUME;
    }

    @Override
    public String getUUID() {
        return TEST_STORE_UUID;
    }

    @Override
    public long getFreeSpace() {
        return TEST_STORE_FREE_SPACE;
    }

    @Override
    public long getTotalSpace() {
        return TEST_STORE_TOTAL_SPACE;
    }

    @Override
    public long getUsableSpace() {
        return TEST_STORE_USABLE_SPACE;
    }

    @Override
    public long getFreeInodes() {
        return TEST_STORE_FREE_INODES;
    }

    @Override
    public long getTotalInodes() {
        return TEST_STORE_TOTAL_INODES;
    }

    @Override
    public boolean updateAttributes() {
        return true;
    }
}
