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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.core.thing.ThingTypeUID;
import org.openhab.core.thing.type.ChannelTypeUID;

/**
 * The {@link GPIOBindingConstants} class defines common constants, which are
 * used across the whole binding.
 *
 * @author Nils Bauer - Initial contribution
 * @author Martin Dagarin - Pull Up/Down GPIO pin
 * @author Alexander Falkenstern - Add I2C bus support
 */
@NonNullByDefault
public class GPIOBindingConstants {

    private static final String BINDING_ID = "gpio";

    public static final ThingTypeUID THING_TYPE_REMOTE = new ThingTypeUID(BINDING_ID, "remote");
    public final static ThingTypeUID THING_TYPE_I2C_BUS = new ThingTypeUID(BINDING_ID, "i2c-bus");
    public final static ThingTypeUID THING_TYPE_I2C_DEVICE = new ThingTypeUID(BINDING_ID, "i2c-device");
    public final static ThingTypeUID THING_TYPE_MCP23017 = new ThingTypeUID(BINDING_ID, "i2c-mcp23017");

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

    // MCP23017 settings
    public static final Set<Integer> MCP23017_ADDRESSES = Set.of(0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27);
    public static final ChannelTypeUID MCP23017_CHANNEL_TYPE_INPUT = new ChannelTypeUID(BINDING_ID, "mcp23017-input");
    public static final ChannelTypeUID MCP23017_CHANNEL_TYPE_OUTPUT = new ChannelTypeUID(BINDING_ID, "mcp23017-output");
    public static final String MCP23017_PIN_PREFIX = "GP";
    public static final List<String> MCP23017_GPIO_PINS;
    static {
        List<String> buffer = new ArrayList<>();
        "AB".chars().forEachOrdered(bank -> {
            for (int pin = 0; pin < 8; pin++) {
                buffer.add(String.format("%s%s%d", MCP23017_PIN_PREFIX, (char) bank, pin));
            }
        });
        MCP23017_GPIO_PINS = Collections.unmodifiableList(buffer);
    }
}
