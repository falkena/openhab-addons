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
package org.openhab.binding.insteon.internal.transport;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.locks.ReentrantLock;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.eclipse.jetty.client.HttpClient;
import org.openhab.binding.insteon.internal.config.InsteonLegacyNetworkConfiguration;
import org.openhab.binding.insteon.internal.device.InsteonAddress;
import org.openhab.binding.insteon.internal.device.LegacyPollManager;
import org.openhab.binding.insteon.internal.device.LegacyRequestManager;
import org.openhab.binding.insteon.internal.device.database.LegacyModemDBEntry;
import org.openhab.binding.insteon.internal.transport.message.Msg;
import org.openhab.core.io.transport.serial.SerialPortManager;

/**
 * The driver class manages the modem port.
 *
 * @author Bernd Pfrommer - Initial contribution
 * @author Rob Nielsen - Port to openHAB 2 insteon binding
 * @author Jeremy Setton - Rewrite insteon binding
 */
@NonNullByDefault
public class LegacyDriver {
    private LegacyPort port;
    private LegacyDriverListener listener;
    private LegacyPollManager poller;
    private LegacyRequestManager requester;
    private Map<InsteonAddress, LegacyModemDBEntry> modemDBEntries = new HashMap<>();
    private ReentrantLock modemDBEntriesLock = new ReentrantLock();

    public LegacyDriver(InsteonLegacyNetworkConfiguration config, LegacyDriverListener listener, HttpClient httpClient,
            ScheduledExecutorService scheduler, SerialPortManager serialPortManager) {
        this.listener = listener;

        this.port = new LegacyPort(config, this, httpClient, scheduler, serialPortManager);
        this.poller = new LegacyPollManager(scheduler);
        this.requester = new LegacyRequestManager(scheduler);
    }

    public boolean isReady() {
        return port.isRunning();
    }

    public Map<InsteonAddress, LegacyModemDBEntry> lockModemDBEntries() {
        modemDBEntriesLock.lock();
        return modemDBEntries;
    }

    public void unlockModemDBEntries() {
        modemDBEntriesLock.unlock();
    }

    public void addPortListener(LegacyPortListener listener) {
        port.addListener(listener);
    }

    public void removePortListener(LegacyPortListener listener) {
        port.removeListener(listener);
    }

    public void start() {
        port.start();
        poller.start();
        requester.start();
    }

    public void stop() {
        port.stop();
        poller.stop();
        requester.stop();
    }

    public void writeMessage(Msg m) throws IOException {
        port.writeMessage(m);
    }

    public String getPortName() {
        return port.getName();
    }

    public boolean isRunning() {
        return port.isRunning();
    }

    public boolean isMsgForUs(@Nullable InsteonAddress toAddr) {
        return port.getAddress().equals(toAddr);
    }

    public void modemDBComplete(LegacyPort port) {
        if (isModemDBComplete()) {
            listener.driverCompletelyInitialized();
        }
    }

    public boolean isModemDBComplete() {
        return port.isModemDBComplete();
    }

    public void disconnected() {
        listener.disconnected();
    }

    public LegacyPollManager getPollManager() {
        return poller;
    }

    public LegacyRequestManager getRequestManager() {
        return requester;
    }
}
