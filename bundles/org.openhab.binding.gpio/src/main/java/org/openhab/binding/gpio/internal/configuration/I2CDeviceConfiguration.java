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
 * The {@link I2CDeviceConfiguration} is base config class for all I2C devices.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class I2CDeviceConfiguration {
    private String address = "";

    public Integer getAddress() {
        return Integer.decode(address);
    }

    public String getAddressAsHex() {
        return String.format("0x%s", Integer.toHexString(getAddress()).toUpperCase());
    }

    public void setAddress(final Integer address) {
        this.address = String.format("0x%s", Integer.toHexString(address).toUpperCase());
    }
}
