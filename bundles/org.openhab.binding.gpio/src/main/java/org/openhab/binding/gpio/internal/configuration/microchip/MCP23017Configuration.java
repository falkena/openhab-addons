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

import static org.openhab.binding.gpio.internal.configuration.GPIOConfiguration.INVALID_GPIO_ID;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

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
    private Integer interrupt = INVALID_GPIO_ID;
    private String mode = InterruptMode.OPEN_DRAIN.toString();
    private Integer reset = INVALID_GPIO_ID;
    private Boolean resethigh = Boolean.TRUE;

    public enum InterruptMode {
        HIGH("HIGH"),
        LOW("LOW"),
        OPEN_DRAIN("OPEN-DRAIN");

        private final String value;
        private static final Map<String, InterruptMode> INTERRUPT_MODES;
        static {
            final Map<String, InterruptMode> map = new ConcurrentHashMap<>();
            for (final InterruptMode mode : InterruptMode.values()) {
                map.put(mode.value.toUpperCase(), mode);
            }
            INTERRUPT_MODES = Collections.unmodifiableMap(map);
        }

        InterruptMode(String value) {
            this.value = value;
        }

        public static InterruptMode fromString(final String mode) {
            final InterruptMode result = INTERRUPT_MODES.get(mode.toUpperCase());
            return (result == null ? InterruptMode.OPEN_DRAIN : result);
        }

        public static String toString(final InterruptMode mode) {
            return mode.value.toUpperCase();
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

    public Integer getInterruptPin() {
        return interrupt;
    }

    public void setInterruptPin(final Integer pin) {
        this.interrupt = pin;
    }

    public InterruptMode getInterruptPinMode() {
        return InterruptMode.fromString(mode);
    }

    public void setInterruptPinMode(final InterruptMode mode) {
        this.mode = InterruptMode.toString(mode);
    }

    public Integer getResetPin() {
        return reset;
    }

    public void setResetPin(final Integer pin) {
        this.reset = pin;
    }

    public Boolean isResetActiveOnHigh() {
        return resethigh;
    }

    public void setResetActiveOnHigh(final Boolean activehigh) {
        this.resethigh = activehigh;
    }
}
