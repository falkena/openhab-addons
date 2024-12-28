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

import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_AVAILABLE;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_AVAILABLE_PERCENT;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_DESCRIPTION;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NAME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_PARTITION_GROUP;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_TOTAL;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_TYPE;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_USED;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_USED_PERCENT;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_VOLUME_GROUP;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.DEVICE_INDEX_PARAMETER;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.PROPERTY_IDENTIFICATION;
import static org.openhab.binding.systeminfo.internal.handler.SystemInfoHandlerUtilities.getChannelState;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.systeminfo.internal.config.SystemInfoPartitionConfig;
import org.openhab.binding.systeminfo.internal.model.SystemInfoInterface;
import org.openhab.core.library.types.StringType;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.Channel;
import org.openhab.core.thing.ChannelGroupUID;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusDetail;
import org.openhab.core.thing.ThingTypeUID;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.ThingHandlerCallback;
import org.openhab.core.thing.binding.builder.ChannelBuilder;
import org.openhab.core.thing.binding.builder.ThingBuilder;
import org.openhab.core.thing.type.ChannelGroupTypeUID;
import org.openhab.core.types.Command;
import org.openhab.core.types.RefreshType;
import org.openhab.core.types.State;
import org.openhab.core.types.UnDefType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import oshi.hardware.HWDiskStore;
import oshi.hardware.HWPartition;
import oshi.software.os.OSFileStore;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoPartitionHandler extends SystemInfoBaseThingHandler {
    private final Logger logger = LoggerFactory.getLogger(SystemInfoPartitionHandler.class);

    private @Nullable HWPartition partition;
    private final Map<String, OSFileStore> volumes = new HashMap<>();

    public SystemInfoPartitionHandler(Thing thing, SystemInfoInterface systemInfo) {
        super(thing, systemInfo);
    }

    @Override
    public void initialize() {
        final Thing thing = getThing();
        final ThingTypeUID typeUID = thing.getThingTypeUID();
        logger.trace("Initializing thing {} with thing type {}", thing.getUID(), typeUID);

        final Bridge bridge = getBridge();
        final List<Channel> channels = new ArrayList<>(thing.getChannelsOfGroup(CHANNEL_PARTITION_GROUP));
        if ((bridge != null) && (bridge.getHandler() instanceof SystemInfoDriveHandler handler)) {
            final HWDiskStore drive = handler.getDrive();
            final ThingHandlerCallback callback = getCallback();
            if ((drive != null) && (callback != null)) {
                try {
                    final int index = getConfigAs(SystemInfoPartitionConfig.class).getIndex();
                    final HWPartition partition = drive.getPartitions().get(index);
                    final Map<String, String> properties = editProperties();
                    properties.put(PROPERTY_IDENTIFICATION, partition.getIdentification());
                    updateProperties(properties);

                    final List<OSFileStore> stores = systemInfo.getFileStorageList().stream().filter((store) -> {
                        final String uuid = partition.getUuid();
                        return uuid.equalsIgnoreCase(store.getUUID());
                    }).toList();

                    // Remove all channels, except partition ones
                    final ThingUID thingUID = thing.getUID();
                    for (int sIndex = 0; sIndex < stores.size(); sIndex++) {
                        final ChannelGroupUID groupUID = new ChannelGroupUID(thingUID, CHANNEL_VOLUME_GROUP + sIndex);
                        volumes.put(groupUID.getId(), stores.get(sIndex));

                        final ChannelGroupTypeUID groupTypeUID = new ChannelGroupTypeUID(thingUID.getBindingId(),
                                CHANNEL_VOLUME_GROUP);
                        for (final ChannelBuilder cBuilder : callback.createChannelBuilders(groupUID, groupTypeUID)) {
                            cBuilder.withProperties(Map.of(DEVICE_INDEX_PARAMETER, String.valueOf(sIndex)));
                            channels.add(cBuilder.build());
                        }
                    }

                    this.partition = partition;
                    updateStatus(ThingStatus.ONLINE);
                } catch (IndexOutOfBoundsException exception) {
                    partition = null;
                    updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.HANDLER_INITIALIZING_ERROR,
                            "@text/missing-or-invalid-configuration");
                }
            } else {
                partition = null;
                updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.CONFIGURATION_ERROR);
            }
        } else {
            partition = null;
            updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.CONFIGURATION_ERROR);
        }

        final ThingBuilder tBuilder = editThing();
        tBuilder.withoutChannels(thing.getChannels()).withChannels(channels);
        updateThing(tBuilder.build());

        if (ThingStatus.ONLINE.equals(thing.getStatus())) {
            super.initialize();
        }
    }

    @Override
    public void dispose() {
        partition = null;
        volumes.clear();
        super.dispose();
    }

    @Override
    public void handleCommand(ChannelUID channelUID, Command command) {
        if (ThingStatus.ONLINE.equals(thing.getStatus())) {
            if (command instanceof RefreshType) {
                final HWPartition partition = this.partition;
                final String channelGroupId = channelUID.getGroupId();
                if ((partition != null) && (channelGroupId != null)) {
                    if (channelGroupId.equals(CHANNEL_PARTITION_GROUP)) {
                        final State state = switch (channelUID.getIdWithoutGroup()) {
                            case CHANNEL_NAME -> new StringType(partition.getName());
                            case CHANNEL_DESCRIPTION -> new StringType(partition.getIdentification());
                            case CHANNEL_TYPE -> new StringType(partition.getType());
                            default -> UnDefType.UNDEF;
                        };
                        updateState(channelUID, state);
                    } else if (channelGroupId.contains(CHANNEL_VOLUME_GROUP)) {
                        final OSFileStore volume = volumes.get(channelGroupId);
                        if ((volume != null) && volume.updateAttributes()) {
                            final State state = switch (channelUID.getIdWithoutGroup()) {
                                case CHANNEL_NAME -> new StringType(volume.getName());
                                case CHANNEL_DESCRIPTION -> new StringType(volume.getDescription());
                                case CHANNEL_TYPE -> new StringType(volume.getType());
                                case CHANNEL_AVAILABLE, CHANNEL_AVAILABLE_PERCENT ->
                                    getChannelState(channelUID, volume.getFreeSpace(),
                                            volume.getTotalSpace() - volume.getFreeSpace(), volume.getTotalSpace());
                                case CHANNEL_TOTAL -> getChannelState(channelUID, volume.getFreeSpace(),
                                        volume.getTotalSpace() - volume.getFreeSpace(), volume.getTotalSpace());
                                case CHANNEL_USED, CHANNEL_USED_PERCENT ->
                                    getChannelState(channelUID, volume.getFreeSpace(),
                                            volume.getTotalSpace() - volume.getFreeSpace(), volume.getTotalSpace());
                                default -> UnDefType.UNDEF;
                            };
                            updateState(channelUID, state);
                        } else {
                            updateState(channelUID, UnDefType.UNDEF);
                        }
                    } else {
                        updateState(channelUID, UnDefType.UNDEF);
                    }
                } else {
                    final int index = getConfigAs(SystemInfoPartitionConfig.class).getIndex();
                    logger.warn("Unable to refresh channel {} with partition index {}", channelUID, index);
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
