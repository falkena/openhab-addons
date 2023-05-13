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
package org.openhab.binding.gpio.internal.handler;

import static eu.xeli.jpigpio.PigpioException.PI_BAD_FLAGS;
import static eu.xeli.jpigpio.PigpioException.PI_BAD_I2C_ADDR;
import static eu.xeli.jpigpio.PigpioException.PI_BAD_I2C_BUS;
import static eu.xeli.jpigpio.PigpioException.PI_I2C_OPEN_FAILED;
import static eu.xeli.jpigpio.PigpioException.PI_NO_HANDLE;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.gpio.internal.configuration.I2CBusConfiguration;
import org.openhab.binding.gpio.internal.discovery.I2CDeviceDiscoveryService;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusDetail;
import org.openhab.core.thing.binding.BaseBridgeHandler;
import org.openhab.core.thing.binding.BridgeHandler;
import org.openhab.core.thing.binding.ThingHandlerService;
import org.openhab.core.types.Command;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.xeli.jpigpio.PigpioException;

/**
 * Class for the handling of i2c buses.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class I2CBusHandler extends BaseBridgeHandler {
    private final Logger logger = LoggerFactory.getLogger(I2CBusHandler.class);

    private final CommunicationHandler gpioHandler;

    /**
     * Instantiates a new I2C bus bridge handler.
     *
     * @param bridge the thing
     */
    public I2CBusHandler(final Bridge bridge, final CommunicationHandler gpioHandler) {
        super(bridge);
        this.gpioHandler = gpioHandler;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<Class<? extends ThingHandlerService>> getServices() {
        return List.of(I2CDeviceDiscoveryService.class);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void handleCommand(ChannelUID channelUID, Command command) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public synchronized void initialize() {
        final CommunicationHandler gpioHandler = this.gpioHandler;
        if (gpioHandler.isConnected()) {
            final I2CBusConfiguration configuration = getConfigAs(I2CBusConfiguration.class);
            try {
                final int i2cBusHandler = gpioHandler.i2cOpen(configuration.getBusId(), 0x00);
                updateStatus(ThingStatus.ONLINE, ThingStatusDetail.NONE, "I2C bus is initialized.");
                try {
                    gpioHandler.i2cClose(i2cBusHandler);
                } catch (PigpioException ignored) {
                }
            } catch (PigpioException exception) {
                final String message = String.format("Can't open bus %d: %s.", configuration.getBusId(), "Bad I2C bus");
                switch (exception.getErrorCode()) {
                    case PI_BAD_I2C_ADDR, PI_BAD_I2C_BUS, PI_BAD_FLAGS, PI_NO_HANDLE ->
                        updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.CONFIGURATION_ERROR, message);
                    case PI_I2C_OPEN_FAILED ->
                        updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.COMMUNICATION_ERROR, message);
                    default -> updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.NONE, message);
                }
            }
        } else {
            updateStatus(ThingStatus.UNKNOWN, ThingStatusDetail.NOT_YET_READY, "Wait for GPIO handler.");
            scheduler.schedule(this::initialize, 5, TimeUnit.SECONDS);
        }
    }

    @Override
    public synchronized void dispose() {
        super.dispose();
    }

    public I2CBusConfiguration getConfiguration() {
        return getConfigAs(I2CBusConfiguration.class);
    }

    public @Nullable GPIORemoteHandler getRemoteHandler() {
        final Bridge bridge = getBridge();
        if (bridge != null) {
            final BridgeHandler handler = bridge.getHandler();
            if (handler instanceof GPIORemoteHandler) {
                return (GPIORemoteHandler) handler;
            }
        }
        return null;
    }
}
