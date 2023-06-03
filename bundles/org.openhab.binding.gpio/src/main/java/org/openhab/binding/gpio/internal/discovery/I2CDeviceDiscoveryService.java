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

import static org.openhab.binding.gpio.internal.GPIOBindingConstants.I2C_DEVICE_ADDRESS;
import static org.openhab.binding.gpio.internal.GPIOBindingConstants.MCP23017_ADDRESSES;
import static org.openhab.binding.gpio.internal.GPIOBindingConstants.THING_TYPE_I2C_DEVICE;
import static org.openhab.binding.gpio.internal.GPIOBindingConstants.THING_TYPE_MCP23017;

import java.util.Set;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.gpio.internal.configuration.GPIORemoteConfiguration;
import org.openhab.binding.gpio.internal.handler.CommunicationHandler;
import org.openhab.binding.gpio.internal.handler.GPIORemoteHandler;
import org.openhab.binding.gpio.internal.handler.I2CBusHandler;
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
 * Discovery service to scan for I2C devices.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class I2CDeviceDiscoveryService extends AbstractDiscoveryService implements ThingHandlerService {

    private final Logger logger = LoggerFactory.getLogger(I2CDeviceDiscoveryService.class);

    private static final int DISCOVERY_TIMEOUT = 30;
    private static final Set<ThingTypeUID> DISCOVERABLE_THING_TYPES_UIDS = Set.of(THING_TYPE_I2C_DEVICE);

    private @Nullable ScheduledFuture<?> backgroundFuture;

    private @Nullable I2CBusHandler handler;

    public I2CDeviceDiscoveryService() {
        super(DISCOVERABLE_THING_TYPES_UIDS, DISCOVERY_TIMEOUT);
    }

    @Override
    public void setThingHandler(@Nullable ThingHandler handler) {
        if (handler instanceof I2CBusHandler) {
            this.handler = (I2CBusHandler) handler;
        }
    }

    @Override
    public @Nullable ThingHandler getThingHandler() {
        return handler;
    }

    @Override
    public void activate() {
        removeOlderResults(getTimestampOfLastScan());
        super.activate(null);
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
        backgroundFuture = scheduler.scheduleWithFixedDelay(this::process, 1, 30, TimeUnit.SECONDS);
    }

    @Override
    protected void stopBackgroundDiscovery() {
        final ScheduledFuture<?> future = backgroundFuture;
        if ((future != null) && !future.isCancelled()) {
            future.cancel(false);
            backgroundFuture = null;
        }
    }

    private void process() {
        removeOlderResults(getTimestampOfLastScan());

        final I2CBusHandler handler = this.handler;
        final Bridge bridge = handler != null ? handler.getThing() : null;
        final GPIORemoteHandler remote = handler != null ? handler.getRemoteHandler() : null;
        if ((bridge != null) && (bridge.getStatus() == ThingStatus.ONLINE) && (remote != null)) {
            final GPIORemoteConfiguration config = remote.getConfiguration();
            final CommunicationHandler gpio = new CommunicationHandler(config.getHost(), config.getPort());

            // Addresses 0000XXX and 1111XXX are reserved by specification
            final int busId = handler.getConfiguration().getBusId();
            for (int address = 0x08; (address < 0x78) && gpio.isConnected(); address++) {
                if (!gpio.i2cIsDeviceAvailable(busId, address)) {
                    continue;
                }

                final String id = String.format("0x%s", Integer.toHexString(address).toUpperCase());
                try {
                    DiscoveryResultBuilder builder;
                    final ThingUID bridgeUID = bridge.getUID();
                    if (MCP23017_ADDRESSES.contains(address)) {
                        builder = DiscoveryResultBuilder.create(new ThingUID(THING_TYPE_MCP23017, bridgeUID, id));
                        builder = builder.withLabel(String.format("MCP23017_%s", id));
                    } else {
                        builder = DiscoveryResultBuilder.create(new ThingUID(THING_TYPE_I2C_DEVICE, bridgeUID, id));
                        builder = builder.withLabel(String.format("I2C device %s", id));
                    }
                    builder = builder.withProperty(I2C_DEVICE_ADDRESS, id);
                    builder = builder.withRepresentationProperty(I2C_DEVICE_ADDRESS);
                    builder = builder.withBridge(bridge.getUID());
                    thingDiscovered(builder.build());
                } catch (NumberFormatException exception) {
                    logger.warn("Found invalid device {}.", id);
                }
            }
            gpio.disconnect();
        }
    }
}
