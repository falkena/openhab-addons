/**
 * Copyright (c) 2010-2023 Contributors to the openHAB project
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
package org.openhab.binding.gpio.internal.configuration;

import org.eclipse.jdt.annotation.NonNullByDefault;

/**
 * The {@link GPIORemoteConfiguration} class contains fields mapping thing configuration parameters.
 *
 * @author Nils Bauer - Initial contribution
 */
@NonNullByDefault
public class GPIORemoteConfiguration {

    /**
     * Network address of the raspberry pi
     */
    private String host = "127.0.0.1";

    /**
     * Port of pigpio on the remote raspberry pi
     */
    private Integer port = 8888;

    public String getHost() {
        return host;
    }

    public Integer getPort() {
        return port;
    }
}
