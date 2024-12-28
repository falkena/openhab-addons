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
import java.util.Map;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;

import oshi.software.os.OSProcess;
import oshi.software.os.OSThread;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoMockedOSProcess implements OSProcess {
    public static final String TEST_PROCESS_COMMAND_LINE = "Mocked Process Command Line";
    public static final int TEST_PROCESS_ID = 100;
    public static final long TEST_PROCESS_KERNEL_TIME = 1200;
    public static final String TEST_PROCESS_NAME = "Mocked Process Name";
    public static final int TEST_PROCESS_PARENT_ID = 215;
    public static final String TEST_PROCESS_PATH = "Mocked Process Path";
    public static final long TEST_PROCESS_RESIDENT_MEMORY = 1024;
    public static final State TEST_PROCESS_STATE = State.ZOMBIE;
    public static final int TEST_PROCESS_THREAD_COUNT = 10;
    public static final long TEST_PROCESS_VIRTUAL_MEMORY = 5 * TEST_PROCESS_RESIDENT_MEMORY;
    public static final long TEST_PROCESS_USER_TIME = 2400;

    public static final double TEST_PROCESS_LOAD = 63.5;
    public static final long TEST_PROCESS_UP_TIME = TEST_PROCESS_KERNEL_TIME + 2 * TEST_PROCESS_USER_TIME;

    @Override
    public String getName() {
        return TEST_PROCESS_NAME;
    }

    @Override
    public String getPath() {
        return TEST_PROCESS_PATH;
    }

    @Override
    public String getCommandLine() {
        return TEST_PROCESS_COMMAND_LINE;
    }

    @Override
    @NonNullByDefault({})
    public List<String> getArguments() {
        return List.of();
    }

    @Override
    @NonNullByDefault({})
    public Map<String, String> getEnvironmentVariables() {
        return Map.of();
    }

    @Override
    public String getCurrentWorkingDirectory() {
        return "";
    }

    @Override
    public String getUser() {
        return "";
    }

    @Override
    public String getUserID() {
        return "";
    }

    @Override
    public String getGroup() {
        return "";
    }

    @Override
    public String getGroupID() {
        return "";
    }

    @Override
    public State getState() {
        return TEST_PROCESS_STATE;
    }

    @Override
    public int getProcessID() {
        return TEST_PROCESS_ID;
    }

    @Override
    public int getParentProcessID() {
        return TEST_PROCESS_PARENT_ID;
    }

    @Override
    public int getThreadCount() {
        return TEST_PROCESS_THREAD_COUNT;
    }

    @Override
    public int getPriority() {
        return 0;
    }

    @Override
    public long getVirtualSize() {
        return TEST_PROCESS_VIRTUAL_MEMORY;
    }

    @Override
    public long getResidentSetSize() {
        return TEST_PROCESS_RESIDENT_MEMORY;
    }

    @Override
    public long getKernelTime() {
        return TEST_PROCESS_KERNEL_TIME;
    }

    @Override
    public long getUserTime() {
        return TEST_PROCESS_USER_TIME;
    }

    @Override
    public long getUpTime() {
        return TEST_PROCESS_UP_TIME;
    }

    @Override
    public long getStartTime() {
        return 0;
    }

    @Override
    public long getBytesRead() {
        return 0;
    }

    @Override
    public long getBytesWritten() {
        return 0;
    }

    @Override
    public long getOpenFiles() {
        return 0;
    }

    @Override
    public long getSoftOpenFileLimit() {
        return 0;
    }

    @Override
    public long getHardOpenFileLimit() {
        return 0;
    }

    @Override
    public double getProcessCpuLoadCumulative() {
        return 0;
    }

    @Override
    public double getProcessCpuLoadBetweenTicks(@Nullable OSProcess process) {
        return TEST_PROCESS_LOAD / 100.0;
    }

    @Override
    public int getBitness() {
        return 0;
    }

    @Override
    public long getAffinityMask() {
        return 0;
    }

    @Override
    public boolean updateAttributes() {
        return true;
    }

    @Override
    @NonNullByDefault({})
    public List<OSThread> getThreadDetails() {
        return List.of();
    }
}
