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
 * The {@link GPIOConfiguration} class contains fields mapping thing configuration parameters.
 *
 * @author Nils Bauer - Initial contribution
 */
@NonNullByDefault
public class GPIOConfiguration {

    static public Integer INVALID_GPIO_ID = -1;

    /**
     * The id of the gpio pin.
     */
    private Integer gpioId = INVALID_GPIO_ID;

    /**
     * Should the input/output be inverted?
     */
    private Boolean activehigh = Boolean.TRUE;

    public Integer getPin() {
        return gpioId;
    }

    public void setPin(final Integer gpioId) {
        this.gpioId = gpioId;
    }

    public static boolean isPinValid(final Integer gpioId) {
        return !INVALID_GPIO_ID.equals(gpioId);
    }

    public Boolean isActiveOnHigh() {
        return activehigh;
    }

    public void setActiveOnHigh(final Boolean activehigh) {
        this.activehigh = activehigh;
    }
}
