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

import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.BRIDGE_TYPE_DRIVE;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.DEVICE_INDEX_PARAMETER;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.systeminfo.internal.handler.SystemInfoComputerHandler;
import org.openhab.binding.systeminfo.internal.model.SystemInfoInterface;
import org.openhab.core.config.discovery.AbstractDiscoveryService;
import org.openhab.core.config.discovery.DiscoveryResultBuilder;
import org.openhab.core.config.discovery.DiscoveryService;
import org.openhab.core.thing.ThingTypeUID;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.ThingHandler;
import org.openhab.core.thing.binding.ThingHandlerService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoDriveDiscoveryService extends AbstractDiscoveryService
        implements DiscoveryService, ThingHandlerService {
    public static final String DEFAULT_THING_ID = "drive";
    public static final String DEFAULT_THING_LABEL = "Hard drive";

    private final Logger logger = LoggerFactory.getLogger(SystemInfoDriveDiscoveryService.class);

    private static final Set<ThingTypeUID> SUPPORTED_THING_TYPES_UIDS = Set.of(BRIDGE_TYPE_DRIVE);

    private static final int DISCOVERY_TIME_SECONDS = 30;

    private @Nullable SystemInfoComputerHandler handler;
    private @Nullable ScheduledFuture<?> discoveryJob;

    public SystemInfoDriveDiscoveryService() {
        super(SUPPORTED_THING_TYPES_UIDS, DISCOVERY_TIME_SECONDS);
    }

    @Override
    public void setThingHandler(@Nullable ThingHandler handler) {
        if (handler instanceof SystemInfoComputerHandler) {
            this.handler = (SystemInfoComputerHandler) handler;
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
        logger.debug("Starting system drive discovery.");

        final SystemInfoComputerHandler handler = this.handler;
        if (handler != null) {
            final ThingUID bridgeUID = handler.getThing().getUID();
            final SystemInfoInterface systemInfo = handler.getSystemInfo();
            for (int index = 0; index < systemInfo.getHardDriveCount(); index++) {
                final String thingId = String.format("%s%d", DEFAULT_THING_ID, index);
                final ThingUID drive = new ThingUID(BRIDGE_TYPE_DRIVE, bridgeUID, thingId);
                DiscoveryResultBuilder builder = DiscoveryResultBuilder.create(drive);
                builder.withBridge(bridgeUID).withLabel(DEFAULT_THING_LABEL);
                builder.withProperty(DEVICE_INDEX_PARAMETER, index).withRepresentationProperty(DEVICE_INDEX_PARAMETER);
                thingDiscovered(builder.build());
            }
        } else {
            logger.warn("Drive discovery found no valid computer handler.");
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
