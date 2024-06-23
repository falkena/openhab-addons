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

import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_DRIVE_MODEL;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_DRIVE_READS;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_DRIVE_READ_BYTES;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_DRIVE_SERIAL;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_DRIVE_WRITES;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_DRIVE_WRITE_BYTES;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NAME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.PRIORITY_PARAMETER;
import static org.openhab.core.thing.Thing.PROPERTY_MODEL_ID;
import static org.openhab.core.thing.Thing.PROPERTY_SERIAL_NUMBER;

import java.util.Map;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.systeminfo.internal.config.SystemInfoDriveConfig;
import org.openhab.binding.systeminfo.internal.model.SystemInfoInterface;
import org.openhab.core.library.types.DecimalType;
import org.openhab.core.library.types.QuantityType;
import org.openhab.core.library.types.StringType;
import org.openhab.core.library.unit.Units;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.Channel;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusDetail;
import org.openhab.core.thing.binding.BaseBridgeHandler;
import org.openhab.core.thing.binding.BridgeHandler;
import org.openhab.core.types.Command;
import org.openhab.core.types.RefreshType;
import org.openhab.core.types.State;
import org.openhab.core.types.UnDefType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import oshi.hardware.HWDiskStore;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoDriveHandler extends BaseBridgeHandler {
    private final Logger logger = LoggerFactory.getLogger(SystemInfoDriveHandler.class);

    private @Nullable HWDiskStore drive;
    private final SystemInfoInterface systemInfo;

    public SystemInfoDriveHandler(Bridge bridge, SystemInfoInterface systemInfo) {
        super(bridge);
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
        logger.trace("Initializing thing {} with thing type {}", thing.getUID(), thing.getThingTypeUID());

        try {
            final int index = getConfigAs(SystemInfoDriveConfig.class).getIndex();
            final HWDiskStore drive = systemInfo.getHardDriveList().get(index);
            final Map<String, String> properties = editProperties();
            properties.put(PROPERTY_MODEL_ID, drive.getModel());
            properties.put(PROPERTY_SERIAL_NUMBER, drive.getSerial());
            updateProperties(properties);

            this.drive = drive;
            updateStatus(ThingStatus.ONLINE);
            for (final Channel channel : getThing().getChannels()) {
                changeChannelPriority(channel.getUID(), channel.getConfiguration().get(PRIORITY_PARAMETER));
            }
        } catch (IndexOutOfBoundsException exception) {
            drive = null;
            updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.HANDLER_INITIALIZING_ERROR,
                    "@text/missing-or-invalid-configuration");
        }
    }

    @Override
    public void dispose() {
        drive = null;
        super.dispose();
    }

    @Override
    public void handleCommand(ChannelUID channelUID, Command command) {
        if (ThingStatus.ONLINE.equals(thing.getStatus())) {
            if (command instanceof RefreshType) {
                final HWDiskStore drive = this.drive;
                if ((drive != null) && drive.updateAttributes()) {
                    // drive.getSize()
                    // drive.getCurrentQueueLength()
                    // drive.getTransferTime()
                    // drive.getPartitions()
                    // drive.getTimeStamp()
                    final State state = switch (channelUID.getIdWithoutGroup()) {
                        case CHANNEL_DRIVE_MODEL -> new StringType(drive.getModel());
                        case CHANNEL_DRIVE_READS -> new DecimalType(drive.getReads());
                        case CHANNEL_DRIVE_READ_BYTES -> new QuantityType<>(drive.getReadBytes(), Units.BYTE);
                        case CHANNEL_DRIVE_SERIAL -> new StringType(drive.getSerial());
                        case CHANNEL_DRIVE_WRITES -> new DecimalType(drive.getWrites());
                        case CHANNEL_DRIVE_WRITE_BYTES -> new QuantityType<>(drive.getWriteBytes(), Units.BYTE);
                        case CHANNEL_NAME -> new StringType(drive.getName());
                        default -> UnDefType.UNDEF;
                    };
                    updateState(channelUID, state);
                } else {
                    final int index = getConfigAs(SystemInfoDriveConfig.class).getIndex();
                    logger.warn("Unable to refresh channel {} with device index {}", channelUID, index);
                    updateState(channelUID, UnDefType.UNDEF);
                }
            } else {
                logger.warn("Support REFRESH command only, but got {}", command);
            }
        } else {
            logger.debug("Cannot handle command. Thing is not ONLINE.");
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
