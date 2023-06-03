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

import static org.openhab.binding.gpio.internal.GPIOBindingConstants.THING_TYPE_I2C_BUS;
import static org.openhab.binding.gpio.internal.GPIOBindingConstants.THING_TYPE_I2C_DEVICE;
import static org.openhab.binding.gpio.internal.GPIOBindingConstants.THING_TYPE_MCP23017;
import static org.openhab.binding.gpio.internal.GPIOBindingConstants.THING_TYPE_REMOTE;

import java.util.Set;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.gpio.internal.handler.CommunicationHandler;
import org.openhab.binding.gpio.internal.handler.GPIORemoteHandler;
import org.openhab.binding.gpio.internal.handler.I2CBusHandler;
import org.openhab.binding.gpio.internal.handler.I2CDeviceHandler;
import org.openhab.binding.gpio.internal.handler.microchip.MCP23017Handler;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingTypeUID;
import org.openhab.core.thing.binding.BaseThingHandlerFactory;
import org.openhab.core.thing.binding.ThingHandler;
import org.openhab.core.thing.binding.ThingHandlerFactory;
import org.osgi.service.component.annotations.Component;

/**
 * The {@link GPIOHandlerFactory} is responsible for creating things and thing
 * handlers.
 *
 * @author Nils Bauer - Initial contribution
 * @author Alexander Falkenstern - Add I2C bus support
 * @author Alexander Falkenstern - Add abstract I2C device support
 */
@NonNullByDefault
@Component(configurationPid = "binding.gpio", service = ThingHandlerFactory.class)
public class GPIOHandlerFactory extends BaseThingHandlerFactory {
    private static final Set<ThingTypeUID> SUPPORTED_THING_TYPES_UIDS = Set.of(THING_TYPE_REMOTE, THING_TYPE_I2C_BUS,
            THING_TYPE_I2C_DEVICE, THING_TYPE_MCP23017);

    private final CommunicationHandler remote = new CommunicationHandler();

    @Override
    public boolean supportsThingType(ThingTypeUID thingTypeUID) {
        return SUPPORTED_THING_TYPES_UIDS.contains(thingTypeUID);
    }

    @Override
    protected @Nullable ThingHandler createHandler(Thing thing) {
        final ThingTypeUID thingTypeUID = thing.getThingTypeUID();
        if (THING_TYPE_REMOTE.equals(thingTypeUID) && (thing instanceof Bridge)) {
            return new GPIORemoteHandler((Bridge) thing, remote);
        } else if (THING_TYPE_I2C_BUS.equals(thingTypeUID) && (thing instanceof Bridge)) {
            return new I2CBusHandler((Bridge) thing, remote);
        } else if (THING_TYPE_MCP23017.equals(thingTypeUID)) {
            return new MCP23017Handler(thing, remote);
        } else if (THING_TYPE_I2C_DEVICE.equals(thingTypeUID)) {
            return new I2CDeviceHandler(thing, remote);
        }

        return null;
    }
}
