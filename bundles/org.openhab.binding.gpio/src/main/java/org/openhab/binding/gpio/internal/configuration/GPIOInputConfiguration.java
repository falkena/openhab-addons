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
 * The {@link GPIOInputConfiguration} class contains fields mapping thing configuration parameters.
 *
 * @author Nils Bauer - Initial contribution
 * @author Martin Dagarin - Pull Up/Down GPIO pin
 */
@NonNullByDefault
public class GPIOInputConfiguration extends GPIOConfiguration {
    /**
     * Time in ms to double check if value hasn't changed
     */
    public Integer debounce = 10;

    /**
     * Setup a pullup resistor on the GPIO pin
     * OFF = PI_PUD_OFF, DOWN = PI_PUD_DOWN, UP = PI_PUD_UP
     */
    private String pullupdown = "OFF";

    // Pull Up/Down modes
    public enum PullUpDownConfig {
        OFF("OFF"),
        DOWN("DOWN"),
        UP("UP");

        private final String value;

        PullUpDownConfig(String value) {
            this.value = value;
        }

        String getValue() {
            return value;
        }
    }

    public PullUpDownConfig getPullUpDownConfig() {
        return PullUpDownConfig.valueOf(pullupdown.toUpperCase());
    }

    public void setPullUpDownConfig(final PullUpDownConfig interrupt) {
        this.pullupdown = interrupt.toString();
    }
}
