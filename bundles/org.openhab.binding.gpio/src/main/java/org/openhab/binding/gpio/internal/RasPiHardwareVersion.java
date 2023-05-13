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

import java.math.BigInteger;
import java.util.Map;

import org.eclipse.jdt.annotation.NonNullByDefault;

/**
 * The {@link RasPiHardwareVersion} class decodes revision codes.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public final class RasPiHardwareVersion {

    public record Hardware(String model, String revision, String manufacturer, String memory) {
    }

    public static Hardware UNKNOWN = new Hardware("Unknown", "Unknown", "Unknown", "Unknown");
    private static final Map<Integer, Hardware> oldStyle = Map.ofEntries(
            Map.entry(0x0002, new Hardware("B", "1.0", "Egoman", "256MB")),
            Map.entry(0x0003, new Hardware("B", "1.0", "Egoman", "256MB")),
            Map.entry(0x0004, new Hardware("B", "2.0", "Sony UK", "256MB")),
            Map.entry(0x0005, new Hardware("B", "2.0", "Qisda", "256MB")),
            Map.entry(0x0006, new Hardware("B", "2.0", "Egoman", "256MB")),
            Map.entry(0x0007, new Hardware("A", "2.0", "Egoman", "256MB")),
            Map.entry(0x0008, new Hardware("A", "2.0", "Sony UK", "256MB")),
            Map.entry(0x0009, new Hardware("A", "2.0", "Qisda", "256MB")),
            Map.entry(0x000D, new Hardware("B", "2.0", "Egoman", "512MB")),
            Map.entry(0x000E, new Hardware("B", "2.0", "Sony UK", "512MB")),
            Map.entry(0x000F, new Hardware("B", "2.0", "Egoman", "512MB")),
            Map.entry(0x0010, new Hardware("B+", "1.2", "Sony UK", "512MB")),
            Map.entry(0x0011, new Hardware("CM1", "1.0", "Sony UK", "512MB")),
            Map.entry(0x0012, new Hardware("A+", "1.1", "Sony UK", "256MB")),
            Map.entry(0x0013, new Hardware("B+", "1.2", "Embest", "512MB")),
            Map.entry(0x0014, new Hardware("CM1", "1.0", "Embest", "512MB")),
            Map.entry(0x0015, new Hardware("A+", "1.1", "Embest", "256MB/512MB")));

    private static final Map<Integer, String> memory = Map.ofEntries(Map.entry(0, "256MB"), Map.entry(0x1, "512MB"),
            Map.entry(0x2, "1GB"), Map.entry(0x3, "2GB"), Map.entry(0x4, "4GB"), Map.entry(0x5, "8GB"));

    private static final Map<Integer, String> manufacturer = Map.ofEntries(Map.entry(0x0, "Sony UK"),
            Map.entry(0x1, "Egoman"), Map.entry(0x2, "Embest"), Map.entry(0x3, "Sony Japan"), Map.entry(0x4, "Embest"),
            Map.entry(0x5, "Stadium"));
    private static final Map<Integer, String> model = Map.ofEntries(Map.entry(0x0, "A"), Map.entry(0x1, "B"),
            Map.entry(0x2, "A+"), Map.entry(0x3, "B+"), Map.entry(0x4, "2B"), Map.entry(0x5, "Alpha (early prototype)"),
            Map.entry(0x6, "CM1"), Map.entry(0x8, "3B"), Map.entry(0x9, "Zero"), Map.entry(0xA, "CM3"),
            Map.entry(0xC, "Zero W"), Map.entry(0xD, "3B+"), Map.entry(0xE, "3A+"), Map.entry(0xF, "Internal use only"),
            Map.entry(0x10, "CM3+"), Map.entry(0x11, "4B"), Map.entry(0x12, "Zero 2 W"), Map.entry(0x13, "400"),
            Map.entry(0x14, "CM4"), Map.entry(0x15, "CM4S"));

    public static Hardware getHardwareVersion(final Integer code) {
        final BigInteger value = BigInteger.valueOf(code);
        if (value.testBit(23)) {
            // MMM (bits 20-22) --> Memory
            final Integer kMemory = (value.and(BigInteger.valueOf(0x700000)).intValue() >> 20);

            // CCCC (bits 16-19) --> Manufacturer
            final Integer kManufacture = (value.and(BigInteger.valueOf(0x0F0000)).intValue() >> 16);

            // TTTTTTTT (bits 4-11) --> Type
            final Integer kModel = (value.and(BigInteger.valueOf(0x000FF0)).intValue() >> 4);

            // RRRR (bits 0-3) --> Revision
            final Integer revision = value.and(BigInteger.valueOf(0x00000F)).intValue();

            return new Hardware(model.getOrDefault(kModel, "Unknown"), String.format("1.%d", revision),
                    manufacturer.getOrDefault(kManufacture, "Unknown"), memory.getOrDefault(kMemory, "Unknown"));
        } else {
            return oldStyle.getOrDefault(value.intValue(), UNKNOWN);
        }
    }
}
