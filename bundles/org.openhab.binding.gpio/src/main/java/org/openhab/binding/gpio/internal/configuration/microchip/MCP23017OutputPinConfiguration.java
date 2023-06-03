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
 * The {@link MCP23017OutputPinConfiguration} contains for MCP23017 output pin configuration.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class MCP23017OutputPinConfiguration extends MCP23017PinConfiguration {
    private String defval = "OFF";

    public Boolean isDefaultActive() {
        return "ON".equalsIgnoreCase(defval.toUpperCase().trim());
    }

    public void setDefaultActive(final Boolean active) {
        this.defval = active ? "ON" : "OFF";
    }
}
