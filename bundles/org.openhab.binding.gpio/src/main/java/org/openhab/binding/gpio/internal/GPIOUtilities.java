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

package org.openhab.binding.gpio.internal;

import org.eclipse.jdt.annotation.NonNullByDefault;

@NonNullByDefault
public class GPIOUtilities {
    public static boolean getBit(byte data, int position) throws IllegalArgumentException {
        if ((0 <= position) && (position <= 7)) {
            return ((data & (1 << position)) > 0);
        } else {
            throw new IllegalArgumentException("Invalid bit configuration");
        }
    }

    public static byte setBit(byte data, int position, boolean value) throws IllegalArgumentException {
        if ((0 <= position) && (position <= 7)) {
            final int mask = (1 << position);
            return (byte) (value ? data | mask : data & ~mask);
        } else {
            throw new IllegalArgumentException("Invalid bit configuration");
        }
    }
}
