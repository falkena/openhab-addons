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

import static org.openhab.binding.systeminfo.test.data.SystemInfoMockedOSProcess.TEST_PROCESS_ID;

import java.util.Comparator;
import java.util.List;
import java.util.function.Predicate;

import org.eclipse.jdt.annotation.NonNullByDefault;

import oshi.software.os.FileSystem;
import oshi.software.os.InternetProtocolStats;
import oshi.software.os.NetworkParams;
import oshi.software.os.OSProcess;
import oshi.software.os.OSThread;
import oshi.software.os.OperatingSystem;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoMockedOperatingSystem implements OperatingSystem {
    public static final String TEST_SYSTEM_BUILD_NUMBER = "Mocked Operating System Build";
    public static final String TEST_SYSTEM_CODE_NAME = "Mocked Operating System Codename";
    public static final String TEST_SYSTEM_VERSION = "Mocked Operating System";

    public static final String TEST_SYSTEM_FAMILY = "Mocked Operating System Family";
    public static final String TEST_SYSTEM_MANUFACTURER = "Mocked Operating System Manufacturer";
    public static final int TEST_SYSTEM_THREAD_COUNT = 16;
    public static final int TEST_SYSTEM_THREAD_ID = 1;
    public static final long TEST_SYSTEM_UPTIME = 3600;

    public static final int TEST_SYSTEM_PROCESS_ID = TEST_PROCESS_ID;

    private final FileSystem fileSystem = new SystemInfoMockedFileSystem();
    private final NetworkParams networkParams = new SystemInfoMockedNetworkParams();
    private final OSProcess process = new SystemInfoMockedOSProcess();
    private final InternetProtocolStats internetStatistics = new SystemInfoMockedInternetProtocolStats();
    private final OSThread thread = new SystemInfoMockedOSThread();
    private final OSVersionInfo version = new OSVersionInfo(TEST_SYSTEM_VERSION, TEST_SYSTEM_CODE_NAME,
            TEST_SYSTEM_BUILD_NUMBER);

    @Override
    public String getFamily() {
        return TEST_SYSTEM_FAMILY;
    }

    @Override
    public String getManufacturer() {
        return TEST_SYSTEM_MANUFACTURER;
    }

    @Override
    public OSVersionInfo getVersionInfo() {
        return version;
    }

    @Override
    public FileSystem getFileSystem() {
        return fileSystem;
    }

    @Override
    public InternetProtocolStats getInternetProtocolStats() {
        return internetStatistics;
    }

    @Override
    @NonNullByDefault({})
    public List<OSProcess> getProcesses(Predicate<OSProcess> filter, Comparator<OSProcess> sort, int limit) {
        return List.of();
    }

    @Override
    public OSProcess getProcess(int pid) {
        return process;
    }

    @Override
    @NonNullByDefault({})
    public List<OSProcess> getChildProcesses(int parentPid, Predicate<OSProcess> filter, Comparator<OSProcess> sort,
            int limit) {
        return List.of();
    }

    @Override
    @NonNullByDefault({})
    public List<OSProcess> getDescendantProcesses(int parentPid, Predicate<OSProcess> filter,
            Comparator<OSProcess> sort, int limit) {
        return List.of();
    }

    @Override
    public int getProcessId() {
        return TEST_SYSTEM_PROCESS_ID;
    }

    @Override
    public int getProcessCount() {
        return 0;
    }

    @Override
    public int getThreadId() {
        return TEST_SYSTEM_THREAD_ID;
    }

    @Override
    public OSThread getCurrentThread() {
        return thread;
    }

    @Override
    public int getThreadCount() {
        return TEST_SYSTEM_THREAD_COUNT;
    }

    @Override
    public int getBitness() {
        return 0;
    }

    @Override
    public long getSystemUptime() {
        return TEST_SYSTEM_UPTIME;
    }

    @Override
    public long getSystemBootTime() {
        return 0;
    }

    @Override
    public NetworkParams getNetworkParams() {
        return networkParams;
    }
}
