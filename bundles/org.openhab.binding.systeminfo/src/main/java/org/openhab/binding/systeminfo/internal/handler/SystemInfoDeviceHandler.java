/*
 * Copyright (c) 2010-2025 Contributors to the openHAB project
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
package org.openhab.binding.systeminfo.internal.handler;

import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.PRIORITY_PARAMETER;

import java.util.Map;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.binding.systeminfo.internal.model.SystemInfoInterface;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.Channel;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.binding.BaseThingHandler;
import org.openhab.core.thing.binding.BridgeHandler;
import org.openhab.core.types.RefreshType;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public abstract class SystemInfoDeviceHandler extends BaseThingHandler {

    protected final SystemInfoInterface systemInfo;

    public SystemInfoDeviceHandler(Thing thing, SystemInfoInterface systemInfo) {
        super(thing);
        this.systemInfo = systemInfo;
    }

    @Override
    public void handleConfigurationUpdate(Map<String, Object> configuration) {
        super.handleConfigurationUpdate(configuration);
        for (final Channel channel : getThing().getChannels()) {
            changeChannelPriority(channel.getUID(), channel.getConfiguration().get(PRIORITY_PARAMETER));
        }
    }

    @Override
    public void thingUpdated(Thing thing) {
        for (final Channel newChannel : thing.getChannels()) {
            final Channel oldChannel = getThing().getChannel(newChannel.getUID());
            final Object newPriority = newChannel.getConfiguration().get(PRIORITY_PARAMETER);
            if ((oldChannel != null) && (newPriority != null)) {
                if (!newPriority.equals(oldChannel.getConfiguration().get(PRIORITY_PARAMETER))) {
                    changeChannelPriority(newChannel.getUID(), newPriority);
                }
            }
        }
        super.thingUpdated(thing);
    }

    @Override
    public void initialize() {
        final Thing thing = getThing();
        if (ThingStatus.ONLINE.equals(thing.getStatus())) {
            for (final Channel channel : thing.getChannels()) {
                changeChannelPriority(channel.getUID(), channel.getConfiguration().get(PRIORITY_PARAMETER));
            }
        }
    }

    private void changeChannelPriority(final ChannelUID channelUID, final Object priority) {
        final Bridge bridge = getBridge();
        if ((bridge != null) && (priority instanceof String)) {
            final BridgeHandler handler = bridge.getHandler();
            if (handler instanceof SystemInfoComputerHandler bridgeHandler) {
                bridgeHandler.changeChannelPriority(channelUID, (String) priority);
                handleCommand(channelUID, RefreshType.REFRESH);
            }
        }
    }
}
