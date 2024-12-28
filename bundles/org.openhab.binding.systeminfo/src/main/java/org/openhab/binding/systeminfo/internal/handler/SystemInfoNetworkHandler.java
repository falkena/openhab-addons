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

import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_DESCRIPTION;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NAME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NETWORK_IP;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NETWORK_MAC;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NETWORK_RECEIVED;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NETWORK_RECEIVED_BYTES;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NETWORK_SENT;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NETWORK_SENT_BYTES;
import static org.openhab.core.thing.Thing.PROPERTY_MAC_ADDRESS;

import java.util.Map;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.systeminfo.internal.config.SystemInfoNetworkConfig;
import org.openhab.binding.systeminfo.internal.model.SystemInfoInterface;
import org.openhab.core.library.types.DecimalType;
import org.openhab.core.library.types.QuantityType;
import org.openhab.core.library.types.StringType;
import org.openhab.core.library.unit.Units;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusDetail;
import org.openhab.core.types.Command;
import org.openhab.core.types.RefreshType;
import org.openhab.core.types.State;
import org.openhab.core.types.UnDefType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import oshi.hardware.NetworkIF;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoNetworkHandler extends SystemInfoBaseThingHandler {
    private final Logger logger = LoggerFactory.getLogger(SystemInfoNetworkHandler.class);

    private @Nullable NetworkIF adapter;

    public SystemInfoNetworkHandler(Thing thing, SystemInfoInterface systemInfo) {
        super(thing, systemInfo);
    }

    @Override
    public void initialize() {
        final Thing thing = getThing();
        logger.trace("Initializing thing {} with thing type {}", thing.getUID(), thing.getThingTypeUID());

        final SystemInfoNetworkConfig config = getConfigAs(SystemInfoNetworkConfig.class);
        systemInfo.getNetworkInterfaceList().stream().filter(entry -> {
            final String name = config.getAdapterName();
            return name.equalsIgnoreCase(entry.getName());
        }).findFirst().ifPresentOrElse((adapter) -> {
            final Map<String, String> properties = editProperties();
            properties.put(PROPERTY_MAC_ADDRESS, adapter.getMacaddr().toUpperCase());
            updateProperties(properties);
            config.setIndex(adapter.getIndex());

            this.adapter = adapter;
            updateStatus(ThingStatus.ONLINE);
            super.initialize();
        }, () -> {
            this.adapter = null;
            updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.HANDLER_INITIALIZING_ERROR,
                    "@text/missing-or-invalid-configuration");
        });
    }

    @Override
    public void dispose() {
        adapter = null;
        super.dispose();
    }

    @Override
    public void handleCommand(ChannelUID channelUID, Command command) {
        if (ThingStatus.ONLINE.equals(thing.getStatus())) {
            if (command instanceof RefreshType) {
                final NetworkIF adapter = this.adapter;
                if ((adapter != null) && adapter.updateAttributes()) {
                    final State state = switch (channelUID.getIdWithoutGroup()) {
                        case CHANNEL_NETWORK_RECEIVED -> new DecimalType(adapter.getPacketsRecv());
                        case CHANNEL_NETWORK_RECEIVED_BYTES -> new QuantityType<>(adapter.getBytesRecv(), Units.BYTE);
                        case CHANNEL_NETWORK_SENT -> new DecimalType(adapter.getPacketsSent());
                        case CHANNEL_NETWORK_SENT_BYTES -> new QuantityType<>(adapter.getBytesSent(), Units.BYTE);
                        case CHANNEL_NETWORK_IP -> {
                            final String[] addresses = adapter.getIPv4addr();
                            yield addresses.length > 0 ? new StringType(addresses[0]) : UnDefType.UNDEF;
                        }
                        case CHANNEL_NAME -> new StringType(adapter.getName());
                        case CHANNEL_DESCRIPTION -> new StringType(adapter.getDisplayName());
                        case CHANNEL_NETWORK_MAC -> new StringType(adapter.getMacaddr().toUpperCase());
                        default -> UnDefType.UNDEF;
                    };
                    updateState(channelUID, state);
                } else {
                    final String name = getConfigAs(SystemInfoNetworkConfig.class).getAdapterName();
                    logger.warn("Unable to refresh channel {} with device name {}", channelUID, name);
                    updateState(channelUID, UnDefType.UNDEF);
                }
            } else {
                logger.warn("Support REFRESH command only, but got {}", command);
            }
        } else {
            logger.debug("Cannot handle command. Thing is not ONLINE.");
        }
    }
}
