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

import oshi.hardware.HWDiskStore;
import oshi.hardware.HWPartition;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoMockedHWDiskStore implements HWDiskStore {
    private final HWPartition partition = new HWPartition(TEST_PARTITION_IDENTIFICATION, TEST_PARTITION_NAME,
            TEST_PARTITION_TYPE, TEST_PARTITION_UUID, TEST_PARTITION_SIZE, TEST_PARTITION_MAJOR, TEST_PARTITION_MINOR,
            TEST_PARTITION_MOUNT);

    public static final String TEST_DRIVE_NAME = "Mocked Drive Name";
    public static final String TEST_DRIVE_MODEL = "Mocked Drive Model";
    public static final String TEST_DRIVE_SERIAL = "Mocked Drive Serial Number";
    public static final long TEST_DRIVE_READS = 0;
    public static final long TEST_DRIVE_READ_BYTES = 512;
    public static final long TEST_DRIVE_SIZE = 1024;
    public static final long TEST_DRIVE_WRITES = 1;
    public static final long TEST_DRIVE_WRITE_BYTES = 512;

    public static final String TEST_PARTITION_IDENTIFICATION = "Mocked Partition";
    public static final String TEST_PARTITION_MOUNT = "Mocked Partition Mount";
    public static final String TEST_PARTITION_NAME = "Mocked Partition Name";
    public static final String TEST_PARTITION_TYPE = "Mocked Partition Type";
    public static final String TEST_PARTITION_UUID = "Mocked Partition UUID";
    public static final long TEST_PARTITION_SIZE = TEST_DRIVE_SIZE / 2;
    public static final int TEST_PARTITION_MAJOR = 1;
    public static final int TEST_PARTITION_MINOR = 0;

    @Override
    public String getName() {
        return TEST_DRIVE_NAME;
    }

    @Override
    public String getModel() {
        return TEST_DRIVE_MODEL;
    }

    @Override
    public String getSerial() {
        return TEST_DRIVE_SERIAL;
    }

    @Override
    public long getSize() {
        return TEST_DRIVE_SIZE;
    }

    @Override
    public long getReads() {
        return TEST_DRIVE_READS;
    }

    @Override
    public long getReadBytes() {
        return TEST_DRIVE_READ_BYTES;
    }

    @Override
    public long getWrites() {
        return TEST_DRIVE_WRITES;
    }

    @Override
    public long getWriteBytes() {
        return TEST_DRIVE_WRITE_BYTES;
    }

    @Override
    public long getCurrentQueueLength() {
        return 0;
    }

    @Override
    public long getTransferTime() {
        return 0;
    }

    @Override
    @NonNullByDefault({})
    public List<HWPartition> getPartitions() {
        return List.of(partition);
    }

    @Override
    public long getTimeStamp() {
        return 0;
    }

    @Override
    public boolean updateAttributes() {
        return true;
    }
}
