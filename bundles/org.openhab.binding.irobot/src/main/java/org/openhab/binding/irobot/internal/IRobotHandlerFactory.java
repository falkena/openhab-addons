/**
 * Copyright (c) 2010-2021 Contributors to the openHAB project
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
package org.openhab.binding.irobot.internal;

import static org.openhab.binding.irobot.internal.IRobotBindingConstants.THING_TYPE_BRAAVA_M;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.THING_TYPE_ROOMBA_9;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.THING_TYPE_ROOMBA_I;

import java.util.Set;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.irobot.internal.handler.BraavaMModelsHandler;
import org.openhab.binding.irobot.internal.handler.Roomba9ModelsHandler;
import org.openhab.binding.irobot.internal.handler.RoombaIModelsHandler;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingTypeUID;
import org.openhab.core.thing.binding.BaseThingHandlerFactory;
import org.openhab.core.thing.binding.ThingHandler;
import org.openhab.core.thing.binding.ThingHandlerFactory;
import org.osgi.service.component.annotations.Component;

/**
 * The {@link IRobotHandlerFactory} is responsible for creating things and thing handlers.
 *
 * @author hkuhn42 - Initial contribution
 * @author Pavel Fedin - rename and update
 * @author Alexander Falkenstern - add braava M6 handler
 */
@Component(configurationPid = "binding.irobot", service = ThingHandlerFactory.class)
@NonNullByDefault
public class IRobotHandlerFactory extends BaseThingHandlerFactory {

    // @formatter:off
    public static final Set<ThingTypeUID> SUPPORTED_THING_TYPES_UIDS = Set.of(
            THING_TYPE_BRAAVA_M,
            THING_TYPE_ROOMBA_9,
            THING_TYPE_ROOMBA_I
    );
    // @formatter:on

    @Override
    public boolean supportsThingType(ThingTypeUID thingTypeUID) {
        return SUPPORTED_THING_TYPES_UIDS.contains(thingTypeUID);
    }

    @Override
    protected @Nullable ThingHandler createHandler(Thing thing) {
        ThingTypeUID thingTypeUID = thing.getThingTypeUID();

        if (thingTypeUID.equals(THING_TYPE_BRAAVA_M)) {
            return new BraavaMModelsHandler(thing);
        } else if (thingTypeUID.equals(THING_TYPE_ROOMBA_9)) {
            return new Roomba9ModelsHandler(thing);
        } else if (thingTypeUID.equals(THING_TYPE_ROOMBA_I)) {
            return new RoombaIModelsHandler(thing);
        }

        return null;
    }
}
