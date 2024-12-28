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

import java.net.NetworkInterface;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;

import oshi.hardware.NetworkIF;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoMockedNetworkInterface implements NetworkIF {
    public static final String TEST_NETWORK_DESCRIPTION = "Mocked Adapter Description";
    public static final String TEST_NETWORK_IP = "192.168.1.0";
    public static final String TEST_NETWORK_MAC = "00-11-22-33-44-55";
    public static final String TEST_NETWORK_NAME = "Mocked Adapter Name";
    public static final long TEST_NETWORK_RECEIVED = 0;
    public static final long TEST_NETWORK_RECEIVED_BYTES = 512;
    public static final long TEST_NETWORK_SENT = 1024;
    public static final long TEST_NETWORK_SENT_BYTES = 512;

    @Override
    public String getName() {
        return TEST_NETWORK_NAME;
    }

    @Override
    public int getIndex() {
        return 0;
    }

    @Override
    public String getDisplayName() {
        return TEST_NETWORK_DESCRIPTION;
    }

    @Override
    public long getMTU() {
        return 0;
    }

    @Override
    public String getMacaddr() {
        return TEST_NETWORK_MAC;
    }

    @Override
    public String[] getIPv4addr() {
        return new String[] { TEST_NETWORK_IP };
    }

    @Override
    public Short[] getSubnetMasks() {
        return new Short[0];
    }

    @Override
    public String[] getIPv6addr() {
        return new String[0];
    }

    @Override
    public Short[] getPrefixLengths() {
        return new Short[0];
    }

    @Override
    public long getPacketsRecv() {
        return TEST_NETWORK_RECEIVED;
    }

    @Override
    public long getBytesRecv() {
        return TEST_NETWORK_RECEIVED_BYTES;
    }

    @Override
    public long getPacketsSent() {
        return TEST_NETWORK_SENT;
    }

    @Override
    public long getBytesSent() {
        return TEST_NETWORK_SENT_BYTES;
    }

    @Override
    public long getInErrors() {
        return 0;
    }

    @Override
    public long getOutErrors() {
        return 0;
    }

    @Override
    public long getInDrops() {
        return 0;
    }

    @Override
    public long getCollisions() {
        return 0;
    }

    @Override
    public long getSpeed() {
        return 0;
    }

    @Override
    public long getTimeStamp() {
        return 0;
    }

    @Override
    public boolean isKnownVmMacAddr() {
        return false;
    }

    @Override
    public @Nullable NetworkInterface queryNetworkInterface() {
        return null;
    }

    @Override
    public boolean updateAttributes() {
        return true;
    }
}
