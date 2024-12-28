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
package org.openhab.binding.systeminfo.internal.config;

import org.eclipse.jdt.annotation.NonNullByDefault;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoProcessConfig extends SystemInfoBaseConfig {
    private String name = CURRENT_PROCESS;
    private Integer pid = -1;

    public static final String CURRENT_PROCESS = "OpenHab";

    public Integer getProcessID() {
        return pid;
    }

    public void setProcessID(final Integer pid) {
        this.pid = pid;
    }

    public String getProcessName() {
        return name;
    }

    public void setProcessName(final String name) {
        this.name = name;
    }
}
