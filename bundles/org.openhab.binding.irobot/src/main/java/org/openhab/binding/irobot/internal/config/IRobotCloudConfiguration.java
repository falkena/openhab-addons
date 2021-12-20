/**
 * Copyright (c) 2021 Alexander Falkenstern
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
package org.openhab.binding.irobot.internal.config;

import static org.openhab.binding.irobot.internal.IRobotBindingConstants.UNKNOWN;

import org.eclipse.jdt.annotation.NonNullByDefault;

/**
 * The {@link IRobotCloudConfiguration} is a class for IRobot thing configuration
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class IRobotCloudConfiguration {
    private String email = UNKNOWN;
    private String password = UNKNOWN;
    private Integer timeout = 10;

    public String getEMail() {
        return email;
    }

    public void setEMail(final String email) {
        this.email = email.trim();
    }

    public String getPassword() {
        return password.isBlank() ? UNKNOWN : password;
    }

    public void setPassword(final String password) {
        this.password = password;
    }

    public Integer getTimeout() {
        return timeout;
    }

    public void setTimeout(final Integer timeout) {
        this.timeout = timeout;
    }
}
