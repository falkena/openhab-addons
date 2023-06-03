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
 * The {@link MCPPinConfiguration} contains general pin configuration for
 * Microchip devices.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public abstract class MCPPinConfiguration {

    private String pin = "";

    public String getPin() {
        return pin;
    }

    public void setPin(final String pin) {
        this.pin = pin;
    }

    public abstract int getPinAddress() throws IllegalArgumentException;
}
