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

import java.util.List;

import org.eclipse.jdt.annotation.NonNullByDefault;

import oshi.software.os.InternetProtocolStats;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoMockedInternetProtocolStats implements InternetProtocolStats {
    public static final TcpState TEST_CONNECTION_STATE = TcpState.NONE;
    public static final String TEST_CONNECTION_TYPE = "Mocked Internet Connection Type";
    public static final byte[] TEST_FOREIGN_ADDRESS = { (byte) 0xFD, (byte) 0x2E, (byte) 0xAC, (byte) 0x2B, (byte) 0x6E,
            (byte) 0x6A, (byte) 0x00, (byte) 0x00, (byte) 0x30, (byte) 0x67, (byte) 0x2D, (byte) 0xD9, (byte) 0x33,
            (byte) 0xA1, (byte) 0xFA, (byte) 0x2C };
    public static final int TEST_FOREIGN_PORT = 13506;
    public static final byte[] TEST_LOCAL_ADDRESS = { (byte) 0xC0, (byte) 0xA8, (byte) 0x00, (byte) 0x01 };
    public static final int TEST_LOCAL_PORT = 443;
    public static final int TEST_RECEIVE_QUEUE_LENGTH = 24;
    public static final int TEST_TRANSMIT_QUEUE_LENGTH = 24;
    public static final int TEST_OWNING_PROCESS_ID = TEST_PROCESS_ID;

    public static final long TEST_CONNECTIONS_ACTIVE = 5;
    public static final long TEST_CONNECTIONS_ESTABLISHED = 15;
    public static final long TEST_CONNECTIONS_FAILURES = 2;
    public static final long TEST_CONNECTIONS_PASSIVE = 10;
    public static final long TEST_CONNECTIONS_RESET = 1;
    public static final long TEST_SEGMENTS_ERRORS = 100;
    public static final long TEST_SEGMENTS_SENT = 200;
    public static final long TEST_SEGMENTS_RECEIVED = 150;
    public static final long TEST_SEGMENTS_RESETS = 10;
    public static final long TEST_SEGMENTS_RETRANSMITTED = 50;

    public static final long TEST_DATAGRAMS_ERRORS = 100;
    public static final long TEST_DATAGRAMS_NO_PORT = 25;
    public static final long TEST_DATAGRAMS_SENT = 200;
    public static final long TEST_DATAGRAMS_RECEIVED = 150;

    private final IPConnection connection = new IPConnection(TEST_CONNECTION_TYPE, TEST_LOCAL_ADDRESS, TEST_LOCAL_PORT,
            TEST_FOREIGN_ADDRESS, TEST_FOREIGN_PORT, TEST_CONNECTION_STATE, TEST_TRANSMIT_QUEUE_LENGTH,
            TEST_RECEIVE_QUEUE_LENGTH, TEST_OWNING_PROCESS_ID);

    private final TcpStats tcpStats = new TcpStats(TEST_CONNECTIONS_ESTABLISHED, TEST_CONNECTIONS_ACTIVE,
            TEST_CONNECTIONS_PASSIVE, TEST_CONNECTIONS_FAILURES, TEST_CONNECTIONS_RESET, TEST_SEGMENTS_SENT,
            TEST_SEGMENTS_RECEIVED, TEST_SEGMENTS_RETRANSMITTED, TEST_SEGMENTS_ERRORS, TEST_SEGMENTS_RESETS);

    private final UdpStats udpStats = new UdpStats(TEST_DATAGRAMS_SENT, TEST_DATAGRAMS_RECEIVED, TEST_DATAGRAMS_NO_PORT,
            TEST_DATAGRAMS_ERRORS);

    @Override
    public TcpStats getTCPv4Stats() {
        return tcpStats;
    }

    @Override
    public TcpStats getTCPv6Stats() {
        return tcpStats;
    }

    @Override
    public UdpStats getUDPv4Stats() {
        return udpStats;
    }

    @Override
    public UdpStats getUDPv6Stats() {
        return udpStats;
    }

    @Override
    @NonNullByDefault({})
    public List<IPConnection> getConnections() {
        return List.of(connection);
    }
}
