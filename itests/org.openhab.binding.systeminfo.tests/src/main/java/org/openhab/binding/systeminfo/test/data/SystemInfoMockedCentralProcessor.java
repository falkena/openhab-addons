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
import java.util.stream.LongStream;

import org.eclipse.jdt.annotation.NonNullByDefault;

import oshi.hardware.CentralProcessor;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoMockedCentralProcessor implements CentralProcessor {
    public static final String TEST_CPU_FAMILY = "Mocked CPU Family";
    public static final String TEST_CPU_ID = "Mocked CPU Identifier";
    public static final String TEST_CPU_MODEL = "Mocked CPU Model";
    public static final int TEST_CPU_LOAD = 50;
    public static final int TEST_CPU_LOAD1 = 100;
    public static final int TEST_CPU_LOAD5 = 50;
    public static final int TEST_CPU_LOAD15 = 25;
    public static final String TEST_CPU_NAME = "Mocked CPU Name";
    public static final String TEST_CPU_STEPPING = "Mocked CPU Stepping";
    public static final String TEST_CPU_VENDOR = "Mocked CPU Vendor";
    public static final boolean TEST_CPU_IS_64_BIT = false;
    public static final long TEST_CPU_MAX_FREQUENCY = 3200;
    public static final long TEST_CPU_VENDOR_FREQUENCY = 2 * TEST_CPU_MAX_FREQUENCY;

    public static final int TEST_LOGICAL_CPU_COUNT = 4;
    public static final int TEST_LOGICAL_CPU_FREQUENCY = 2500;

    public static final int TEST_PHYSICAL_CPU_COUNT = 2;

    private final ProcessorIdentifier identifier = new ProcessorIdentifier(TEST_CPU_VENDOR, TEST_CPU_NAME,
            TEST_CPU_FAMILY, TEST_CPU_MODEL, TEST_CPU_STEPPING, TEST_CPU_ID, TEST_CPU_IS_64_BIT,
            TEST_CPU_VENDOR_FREQUENCY);

    @Override
    public ProcessorIdentifier getProcessorIdentifier() {
        return identifier;
    }

    @Override
    public long getMaxFreq() {
        return TEST_CPU_MAX_FREQUENCY;
    }

    @Override
    public long[] getCurrentFreq() {
        return LongStream.generate(() -> TEST_LOGICAL_CPU_FREQUENCY).limit(getLogicalProcessorCount()).toArray();
    }

    @Override
    @NonNullByDefault({})
    public List<LogicalProcessor> getLogicalProcessors() {
        return List.of();
    }

    @Override
    @NonNullByDefault({})
    public List<PhysicalProcessor> getPhysicalProcessors() {
        return List.of();
    }

    @Override
    @NonNullByDefault({})
    public List<ProcessorCache> getProcessorCaches() {
        return List.of();
    }

    @Override
    @NonNullByDefault({})
    public List<String> getFeatureFlags() {
        return List.of();
    }

    @Override
    public long[] getSystemCpuLoadTicks() {
        // User, Nice, System, Idle, IOwait, IRQ, SoftIRQ, and Steal ticks
        return new long[] { 1, 0, 2, 0, 0, 0 };
    }

    @Override
    @NonNullByDefault({})
    public double getSystemCpuLoadBetweenTicks(long[] oldTicks) {
        return TEST_CPU_LOAD / 100.0;
    }

    @Override
    public double[] getSystemLoadAverage(int nelem) {
        return new double[] { TEST_CPU_LOAD1, TEST_CPU_LOAD5, TEST_CPU_LOAD15 };
    }

    @Override
    @NonNullByDefault({})
    public double[] getProcessorCpuLoadBetweenTicks(long[][] oldTicks) {
        return new double[0];
    }

    @Override
    public long[][] getProcessorCpuLoadTicks() {
        return new long[0][];
    }

    @Override
    public int getLogicalProcessorCount() {
        return TEST_LOGICAL_CPU_COUNT;
    }

    @Override
    public int getPhysicalProcessorCount() {
        return TEST_PHYSICAL_CPU_COUNT;
    }

    @Override
    public int getPhysicalPackageCount() {
        return 0;
    }

    @Override
    public long getContextSwitches() {
        return 0;
    }

    @Override
    public long getInterrupts() {
        return 0;
    }
}
