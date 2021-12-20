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

import static org.openhab.binding.irobot.internal.IRobotBindingConstants.BRIDGE_TYPE_CLOUD;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.THING_TYPE_BRAAVA_M;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.THING_TYPE_ROOMBA_9;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.THING_TYPE_ROOMBA_I;

import java.util.Set;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.irobot.internal.handler.BraavaMModelsHandler;
import org.openhab.binding.irobot.internal.handler.IRobotCloudHandler;
import org.openhab.binding.irobot.internal.handler.Roomba9ModelsHandler;
import org.openhab.binding.irobot.internal.handler.RoombaIModelsHandler;
import org.openhab.core.i18n.LocaleProvider;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingTypeUID;
import org.openhab.core.thing.binding.BaseThingHandlerFactory;
import org.openhab.core.thing.binding.ThingHandler;
import org.openhab.core.thing.binding.ThingHandlerFactory;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;

/**
 * The {@link IRobotHandlerFactory} is responsible for creating things and thing handlers.
 *
 * @author hkuhn42 - Initial contribution
 * @author Pavel Fedin - rename and update
 * @author Alexander Falkenstern - add braava M6 handler
 */
@NonNullByDefault
@Component(configurationPid = "binding.irobot", service = ThingHandlerFactory.class)
public class IRobotHandlerFactory extends BaseThingHandlerFactory {

    // @formatter:off
    public static final Set<ThingTypeUID> SUPPORTED_THING_TYPES_UIDS = Set.of(
            BRIDGE_TYPE_CLOUD,
            THING_TYPE_BRAAVA_M,
            THING_TYPE_ROOMBA_9,
            THING_TYPE_ROOMBA_I
    );
    // @formatter:on

    private IRobotChannelContentProvider channelContentProvider;
    private LocaleProvider localeProvider;

    @Activate
    public IRobotHandlerFactory(final @Reference IRobotChannelContentProvider channelContentProvider,
            final @Reference LocaleProvider localeProvider) {
        this.channelContentProvider = channelContentProvider;
        this.localeProvider = localeProvider;
    }

    @Override
    public boolean supportsThingType(ThingTypeUID thingTypeUID) {
        return SUPPORTED_THING_TYPES_UIDS.contains(thingTypeUID);
    }

    @Override
    protected @Nullable ThingHandler createHandler(Thing thing) {
        ThingTypeUID thingTypeUID = thing.getThingTypeUID();

        if (BRIDGE_TYPE_CLOUD.equals(thingTypeUID) && (thing instanceof Bridge)) {
            return new IRobotCloudHandler((Bridge) thing, localeProvider);
        } else if (THING_TYPE_BRAAVA_M.equals(thingTypeUID)) {
            return new BraavaMModelsHandler(thing, channelContentProvider);
        } else if (THING_TYPE_ROOMBA_9.equals(thingTypeUID)) {
            return new Roomba9ModelsHandler(thing, channelContentProvider);
        } else if (THING_TYPE_ROOMBA_I.equals(thingTypeUID)) {
            return new RoombaIModelsHandler(thing, channelContentProvider);
        }

        return null;
    }
}
