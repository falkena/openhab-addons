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
    public static final int TEST_PROCESS_ID = 100;
    public static final int TEST_PROCESS_PARENT_ID = 215;
    public static final State TEST_PROCESS_STATE = State.ZOMBIE;

    @Override
    public String getName() {
        return "";
    }

    @Override
    public String getPath() {
        return "";
    }

    @Override
    public String getCommandLine() {
        return "";
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
        return 0;
    }

    @Override
    public int getPriority() {
        return 0;
    }

    @Override
    public long getVirtualSize() {
        return 0;
    }

    @Override
    public long getResidentSetSize() {
        return 0;
    }

    @Override
    public long getKernelTime() {
        return 0;
    }

    @Override
    public long getUserTime() {
        return 0;
    }

    @Override
    public long getUpTime() {
        return 0;
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
    public double getProcessCpuLoadBetweenTicks(@Nullable OSProcess proc) {
        return 0;
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
        return false;
    }

    @Override
    @NonNullByDefault({})
    public List<OSThread> getThreadDetails() {
        return List.of();
    }
}
