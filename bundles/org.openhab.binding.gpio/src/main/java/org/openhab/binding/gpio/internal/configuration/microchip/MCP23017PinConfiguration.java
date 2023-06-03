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

import static org.openhab.binding.gpio.internal.GPIOBindingConstants.MCP23017_PIN_PREFIX;

import java.util.concurrent.atomic.AtomicReference;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;

/**
 * The {@link MCP23017PinConfiguration} contains common pin configuration for
 * MCP23017 devices.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class MCP23017PinConfiguration extends MCPPinConfiguration {
    final AtomicReference<@Nullable Integer> bank = new AtomicReference<>();
    final AtomicReference<@Nullable Integer> bit = new AtomicReference<>();

    public synchronized int getBank() throws IllegalArgumentException {
        Integer bank = this.bank.get();
        if (bank == null) {
            final String pin = getPin();
            if (pin.startsWith(MCP23017_PIN_PREFIX)) {
                bank = Character.getNumericValue(pin.charAt(2)) - 10;
                if ((bank < 0) || (bank > 1)) {
                    throw new IllegalArgumentException(String.format("Invalid pin configuration found: %s", pin));
                }
            } else {
                throw new IllegalArgumentException(String.format("Invalid pin configuration found: %s", pin));
            }
            this.bank.set(bank);
        }
        return bank;
    }

    @Override
    public synchronized int getPinAddress() throws IllegalArgumentException {
        Integer bit = this.bit.get();
        if (bit == null) {
            final String pin = getPin();
            if (pin.startsWith(MCP23017_PIN_PREFIX)) {
                bit = Integer.parseInt(pin.replaceAll("[^0-9]+", ""));
                if ((bit < 0) || (bit > 7)) {
                    throw new IllegalArgumentException(String.format("Invalid pin configuration found: %s", pin));
                }
            } else {
                throw new IllegalArgumentException(String.format("Invalid pin configuration found: %s", pin));
            }
            this.bit.set(bit);
        }
        return bit;
    }

    public Boolean isActiveHigh() {
        return Boolean.TRUE;
    }

    public Boolean isInterruptEnabled() {
        return Boolean.FALSE;
    }
}
