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
package org.openhab.binding.systeminfo.internal.discovery;

import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.DEVICE_INDEX_PARAMETER;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.PROPERTY_IDENTIFICATION;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.THING_TYPE_PARTITION;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.systeminfo.internal.handler.SystemInfoDriveHandler;
import org.openhab.core.config.discovery.AbstractDiscoveryService;
import org.openhab.core.config.discovery.DiscoveryResultBuilder;
import org.openhab.core.config.discovery.DiscoveryService;
import org.openhab.core.thing.ThingTypeUID;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.ThingHandler;
import org.openhab.core.thing.binding.ThingHandlerService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import oshi.hardware.HWDiskStore;
import oshi.hardware.HWPartition;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoPartitionDiscoveryService extends AbstractDiscoveryService
        implements DiscoveryService, ThingHandlerService {
    private final Logger logger = LoggerFactory.getLogger(SystemInfoPartitionDiscoveryService.class);

    private static final Set<ThingTypeUID> SUPPORTED_THING_TYPES_UIDS = Set.of(THING_TYPE_PARTITION);

    private static final int DISCOVERY_TIME_SECONDS = 30;

    private @Nullable SystemInfoDriveHandler handler;
    private @Nullable ScheduledFuture<?> discoveryJob;

    public SystemInfoPartitionDiscoveryService() {
        super(SUPPORTED_THING_TYPES_UIDS, DISCOVERY_TIME_SECONDS);
    }

    @Override
    public void setThingHandler(@Nullable ThingHandler handler) {
        if (handler instanceof SystemInfoDriveHandler) {
            this.handler = (SystemInfoDriveHandler) handler;
        }
    }

    @Override
    public @Nullable ThingHandler getThingHandler() {
        return handler;
    }

    @Override
    public void activate(@Nullable Map<String, Object> configProperties) {
        super.activate(configProperties);
    }

    @Override
    public void deactivate() {
        super.deactivate();
    }

    @Override
    protected void startScan() {
        logger.debug("Starting system partition discovery.");

        final SystemInfoDriveHandler handler = this.handler;
        final HWDiskStore drive = (handler != null) ? handler.getDrive() : null;
        if ((drive != null) && (handler != null)) {
            final ThingUID bridgeUID = handler.getThing().getUID();
            final List<HWPartition> partitions = drive.getPartitions();
            for (int index = 0; index < partitions.size(); index++) {
                final HWPartition partition = partitions.get(index);
                final String thingId = String.format("partition%d", index);
                final ThingUID thingUID = new ThingUID(THING_TYPE_PARTITION, bridgeUID, thingId);
                final DiscoveryResultBuilder builder = DiscoveryResultBuilder.create(thingUID);
                builder.withBridge(bridgeUID).withLabel("Partition");
                builder.withProperty(DEVICE_INDEX_PARAMETER, index);
                builder.withProperty(PROPERTY_IDENTIFICATION, partition.getIdentification());
                builder.withRepresentationProperty(PROPERTY_IDENTIFICATION);
                thingDiscovered(builder.build());
            }
        } else {
            logger.warn("Partition discovery found no valid computer handler.");
        }
    }

    @Override
    protected void stopScan() {
        logger.debug("Stop system drive discovery.");
        super.stopScan();
        removeOlderResults(getTimestampOfLastScan());
    }

    @Override
    protected void startBackgroundDiscovery() {
        final ScheduledFuture<?> discoveryJob = this.discoveryJob;
        if ((discoveryJob == null) || discoveryJob.isCancelled()) {
            this.discoveryJob = scheduler.scheduleWithFixedDelay(this::startScan, 0, 5, TimeUnit.MINUTES);
        }
    }

    @Override
    protected void stopBackgroundDiscovery() {
        final ScheduledFuture<?> discoveryJob = this.discoveryJob;
        if (discoveryJob != null) {
            discoveryJob.cancel(true);
            this.discoveryJob = null;
        }
    }
}
