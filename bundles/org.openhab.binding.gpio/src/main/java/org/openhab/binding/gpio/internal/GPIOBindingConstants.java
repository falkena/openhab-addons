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
package org.openhab.binding.gpio.internal;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.core.thing.ThingTypeUID;
import org.openhab.core.thing.type.ChannelTypeUID;

/**
 * The {@link GPIOBindingConstants} class defines common constants, which are
 * used across the whole binding.
 *
 * @author Nils Bauer - Initial contribution
 * @author Martin Dagarin - Pull Up/Down GPIO pin
 */
@NonNullByDefault
public class GPIOBindingConstants {

    private static final String BINDING_ID = "gpio";

    public static final ThingTypeUID THING_TYPE_REMOTE = new ThingTypeUID(BINDING_ID, "remote");
    public final static ThingTypeUID THING_TYPE_I2C_BUS = new ThingTypeUID(BINDING_ID, "i2c-bus");
    public final static ThingTypeUID THING_TYPE_I2C_DEVICE = new ThingTypeUID(BINDING_ID, "i2c-device");

    // List of all Thing Type UIDs
    public static final ChannelTypeUID CHANNEL_TYPE_DIGITAL_INPUT = new ChannelTypeUID(BINDING_ID, "digital-input");
    public static final ChannelTypeUID CHANNEL_TYPE_DIGITAL_OUTPUT = new ChannelTypeUID(BINDING_ID, "digital-output");

    // PiGpio config properties
    public static final String HOST = "host";
    public static final String PORT = "port";
    public static final String ACTIVE_HIGH = "activehigh";
    public static final String DEBOUNCING = "debounce";
    public static final String PULLUPDOWN_RESISTOR = "pullupdown";

    // GPIO config properties
    public static final String GPIO_ID = "gpioId";

    // I2C bus config properties
    public static final String I2C_BUS_ID = "id";

    // Device config properties
    public static final String I2C_DEVICE_ADDRESS = "address";
}
