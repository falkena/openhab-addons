/*
 * Copyright (c) 2010-2026 Contributors to the openHAB project
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
package org.openhab.binding.systeminfo.internal;

import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.BINDING_ID;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.BRIDGE_TYPE_COMPUTER_ID;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.BRIDGE_TYPE_COMPUTER_IMPL;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.BRIDGE_TYPE_DRIVE;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.THING_TYPE_NETWORK;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.THING_TYPE_PARTITION;

import java.util.Set;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.systeminfo.internal.handler.SystemInfoComputerHandler;
import org.openhab.binding.systeminfo.internal.handler.SystemInfoDriveHandler;
import org.openhab.binding.systeminfo.internal.handler.SystemInfoNetworkHandler;
import org.openhab.binding.systeminfo.internal.handler.SystemInfoPartitionHandler;
import org.openhab.binding.systeminfo.internal.model.SystemInfoInterface;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingRegistry;
import org.openhab.core.thing.ThingTypeUID;
import org.openhab.core.thing.binding.BaseThingHandlerFactory;
import org.openhab.core.thing.binding.ThingHandler;
import org.openhab.core.thing.binding.ThingHandlerFactory;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;

/**
 * The {@link SystemInfoHandlerFactory} is responsible for creating things and thing
 * handlers.
 *
 * @author Svilen Valkanov - Initial contribution
 * @author Lyubomir Papazov - Pass systeminfo service to the SystemInfoHandler constructor
 * @author Wouter Born - Add null annotations
 * @author Mark Herwege - Add dynamic creation of extra channels
 */
@NonNullByDefault
@Component(service = ThingHandlerFactory.class, configurationPid = "binding.systeminfo")
public class SystemInfoHandlerFactory extends BaseThingHandlerFactory {
    private @Nullable SystemInfoInterface systemInfo;
    private final SystemInfoThingTypeProvider thingTypeProvider;
    private final ThingRegistry thingRegistry;

    private static final Set<ThingTypeUID> SUPPORTED_THING_TYPES_UIDS = Set.of(BRIDGE_TYPE_DRIVE, THING_TYPE_NETWORK,
            THING_TYPE_PARTITION);

    @Activate
    public SystemInfoHandlerFactory(final @Reference SystemInfoThingTypeProvider thingTypeProvider,
            final @Reference ThingRegistry thingRegistry) {
        this.thingTypeProvider = thingTypeProvider;
        this.thingRegistry = thingRegistry;
    }

    @Override
    public boolean supportsThingType(ThingTypeUID thingTypeUID) {
        final String thingTypeId = thingTypeUID.getId();
        return (BINDING_ID.equals(thingTypeUID.getBindingId()) && thingTypeId.startsWith(BRIDGE_TYPE_COMPUTER_ID))
                || SUPPORTED_THING_TYPES_UIDS.contains(thingTypeUID);
    }

    @Override
    protected @Nullable ThingHandler createHandler(Thing thing) {
        final SystemInfoInterface systemInfo = this.systemInfo;
        if (systemInfo != null) {
            final ThingTypeUID thingTypeUID = thing.getThingTypeUID();
            if (thing instanceof Bridge bridge) {
                if (thingTypeUID.getId().startsWith(BRIDGE_TYPE_COMPUTER_ID)) {
                    final SystemInfoThingTypeProvider thingTypeProvider = this.thingTypeProvider;
                    if (thingTypeProvider.getThingType(BRIDGE_TYPE_COMPUTER_IMPL, null) == null) {
                        thingTypeProvider.createThingType(BRIDGE_TYPE_COMPUTER_IMPL);
                        // Save the current channels configs, will be restored after thing type change.
                        thingTypeProvider.storeChannelsConfig(bridge);
                    }
                    return new SystemInfoComputerHandler(bridge, systemInfo, thingTypeProvider, thingRegistry);
                } else if (BRIDGE_TYPE_DRIVE.equals(thingTypeUID)) {
                    return new SystemInfoDriveHandler(bridge, systemInfo);
                }
            } else if (THING_TYPE_NETWORK.equals(thingTypeUID)) {
                return new SystemInfoNetworkHandler(thing, systemInfo);
            } else if (THING_TYPE_PARTITION.equals(thingTypeUID)) {
                return new SystemInfoPartitionHandler(thing, systemInfo);
            }
        }

        return null;
    }

    @Reference
    public void bindSystemInfo(SystemInfoInterface systemInfo) {
        this.systemInfo = systemInfo;
    }

    public void unbindSystemInfo(SystemInfoInterface systemInfo) {
        this.systemInfo = null;
    }
}
