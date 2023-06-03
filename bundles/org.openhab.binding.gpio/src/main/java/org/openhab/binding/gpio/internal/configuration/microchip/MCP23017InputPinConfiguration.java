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

/**
 * The {@link MCP23017InputPinConfiguration} contains for MCP23017 input pin configuration.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class MCP23017InputPinConfiguration extends MCP23017PinConfiguration {

    private Boolean activehigh = Boolean.TRUE;

    private String interrupt = InterruptConfig.OFF.getValue();

    private Boolean pullup = Boolean.FALSE;

    public enum InterruptConfig {
        OFF("OFF"),
        HIGH("HIGH"),
        LOW("LOW"),
        PREVIOUS("PREVIOUS");

        private final String value;

        InterruptConfig(String value) {
            this.value = value;
        }

        String getValue() {
            return value;
        }
    }

    @Override
    public Boolean isActiveHigh() {
        return activehigh;
    }

    public void setActiveHigh(final Boolean activehigh) {
        this.activehigh = activehigh;
    }

    public InterruptConfig getInterruptConfig() {
        return InterruptConfig.valueOf(interrupt.toUpperCase());
    }

    public void setInterruptConfig(final InterruptConfig interrupt) {
        this.interrupt = interrupt.toString();
    }

    @Override
    public Boolean isInterruptEnabled() {
        return (getInterruptConfig() != InterruptConfig.OFF);
    }

    public Boolean isPullUpEnabled() {
        return this.pullup;
    }

    public void setPullUpEnabled(final Boolean enabled) {
        this.pullup = enabled;
    }
}
