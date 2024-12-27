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

import org.eclipse.jdt.annotation.NonNullByDefault;

import oshi.software.os.NetworkParams;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoMockedNetworkParams implements NetworkParams {

    @Override
    public String getHostName() {
        return "";
    }

    @Override
    public String getDomainName() {
        return "";
    }

    @Override
    public String[] getDnsServers() {
        return new String[0];
    }

    @Override
    public String getIpv4DefaultGateway() {
        return "";
    }

    @Override
    public String getIpv6DefaultGateway() {
        return "";
    }
}
