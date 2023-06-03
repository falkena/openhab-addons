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
package org.openhab.binding.gpio.internal.configuration.microchip;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.binding.gpio.internal.configuration.I2CDeviceConfiguration;

/**
 * The {@link MCP23017Configuration} is config class for all MCP23017 devices.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class MCP23017Configuration extends I2CDeviceConfiguration {

    private Integer refresh = 2000;
    private Boolean mirror = Boolean.TRUE;
    private String mode = InterruptMode.OPEN_DRAIN.toString();

    public enum InterruptMode {
        HIGH("HIGH"),
        LOW("LOW"),
        OPEN_DRAIN("OPEN_DRAIN");

        private final String value;

        InterruptMode(String value) {
            this.value = value;
        }
    }

    public Integer getRefreshInterval() {
        return refresh;
    }

    public void setRefreshInterval(final Integer refresh) {
        this.refresh = refresh;
    }

    public Boolean isInterruptMirrored() {
        return mirror;
    }

    public void setInterruptMirrored(final Boolean mirror) {
        this.mirror = mirror;
    }

    public InterruptMode getInterruptPinMode() {
        return InterruptMode.valueOf(mode.toUpperCase());
    }

    public void setInterruptPinMode(final InterruptMode mode) {
        this.mode = mode.toString();
    }
}
