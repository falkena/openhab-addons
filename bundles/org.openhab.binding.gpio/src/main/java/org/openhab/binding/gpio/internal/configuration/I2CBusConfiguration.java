/**
 * Copyright (c) 2010-2023 Alexander Falkenstern
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information.
 *
 * This program and the accompanying materials are made available
 * under the terms of the GNU General Public License v3.0 which is
 * available at https://www.gnu.org/licenses/gpl-3.0-standalone.html
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */
package org.openhab.binding.gpio.internal.configuration;

import org.eclipse.jdt.annotation.NonNullByDefault;

/*
 * The {@link I2CBusConfiguration} is base config class for I2C busses.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class I2CBusConfiguration {

    private Integer id = 0;

    public Integer getBusId() {
        return id;
    }

    public void setBusId(final Integer id) {
        this.id = id;
    }
}
