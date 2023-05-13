/**
 * Copyright (c) 2010-2023 Alexander Falkenstern
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0
 * This Source Code may also be made available under the following
 * Secondary Licenses when the conditions for such availability set
 * forth in the Eclipse Public License, v. 2.0 are satisfied:
 * GNU General Public License, version 3.
 *
 * SPDX-License-Identifier: EPL-2.0 OR GPL-3.0-only
 */
package org.openhab.binding.gpio.internal.discovery;

import static org.openhab.binding.gpio.internal.GPIOBindingConstants.I2C_BUS_ID;
import static org.openhab.binding.gpio.internal.GPIOBindingConstants.THING_TYPE_I2C_BUS;

import java.math.BigDecimal;
import java.util.Set;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.gpio.internal.configuration.GPIORemoteConfiguration;
import org.openhab.binding.gpio.internal.handler.CommunicationHandler;
import org.openhab.binding.gpio.internal.handler.GPIORemoteHandler;
import org.openhab.core.config.discovery.AbstractDiscoveryService;
import org.openhab.core.config.discovery.DiscoveryResultBuilder;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingTypeUID;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.ThingHandler;
import org.openhab.core.thing.binding.ThingHandlerService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Discovery service to scan for I2C buses.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class I2CBusDiscoveryService extends AbstractDiscoveryService implements ThingHandlerService {

    private final Logger logger = LoggerFactory.getLogger(I2CBusDiscoveryService.class);

    private static final int DISCOVERY_TIMEOUT = 30;
    private static final Set<ThingTypeUID> DISCOVERABLE_THING_TYPES_UIDS = Set.of(THING_TYPE_I2C_BUS);

    private @Nullable ScheduledFuture<?> backgroundFuture;

    private @Nullable GPIORemoteHandler handler;

    public I2CBusDiscoveryService() {
        super(DISCOVERABLE_THING_TYPES_UIDS, DISCOVERY_TIMEOUT);
    }

    @Override
    public void setThingHandler(@Nullable ThingHandler handler) {
        if (handler instanceof GPIORemoteHandler) {
            this.handler = (GPIORemoteHandler) handler;
        }
    }

    @Override
    public @Nullable ThingHandler getThingHandler() {
        return handler;
    }

    @Override
    public void deactivate() {
        super.deactivate();
        removeOlderResults(getTimestampOfLastScan());
    }

    @Override
    protected void startScan() {
        scheduler.execute(this::process);
    }

    @Override
    protected void stopScan() {
        super.stopScan();
        removeOlderResults(getTimestampOfLastScan());
    }

    @Override
    protected void startBackgroundDiscovery() {
        stopBackgroundDiscovery();
        backgroundFuture = scheduler.scheduleWithFixedDelay(this::process, 0, 30, TimeUnit.SECONDS);
    }

    @Override
    protected void stopBackgroundDiscovery() {
        ScheduledFuture<?> future = backgroundFuture;
        if (future != null) {
            future.cancel(true);
            backgroundFuture = null;
        }
    }

    private void process() {
        removeOlderResults(getTimestampOfLastScan());

        final GPIORemoteHandler handler = this.handler;
        final Bridge bridge = handler != null ? handler.getThing() : null;

        if ((bridge != null) && (bridge.getStatus() == ThingStatus.ONLINE)) {
            final GPIORemoteConfiguration config = handler.getConfiguration();
            final CommunicationHandler gpio = new CommunicationHandler(config.getHost(), config.getPort());
            // Assume 255 busses are available...
            for (int bus = 0x00; (bus < 0xFF) && gpio.isConnected(); bus++) {
                if (!gpio.i2cIsBusAvailable(bus)) {
                    continue;
                }

                final String id = String.format("i2c-%d", bus);
                try {
                    final ThingUID thingUID = new ThingUID(THING_TYPE_I2C_BUS, bridge.getUID(), id);
                    DiscoveryResultBuilder builder = DiscoveryResultBuilder.create(thingUID);
                    builder = builder.withProperty(I2C_BUS_ID, new BigDecimal(bus));
                    builder = builder.withRepresentationProperty(I2C_BUS_ID);
                    builder = builder.withLabel(id).withBridge(bridge.getUID());
                    thingDiscovered(builder.build());
                } catch (NumberFormatException exception) {
                    logger.warn("Found invalid bus {}.", id);
                }
            }
            gpio.disconnect();
        }
    }
}
