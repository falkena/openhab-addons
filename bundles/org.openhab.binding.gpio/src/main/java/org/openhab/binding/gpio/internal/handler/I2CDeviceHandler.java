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

import java.util.concurrent.TimeUnit;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.gpio.internal.configuration.I2CDeviceConfiguration;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.Channel;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusDetail;
import org.openhab.core.thing.binding.BaseThingHandler;
import org.openhab.core.thing.binding.BridgeHandler;
import org.openhab.core.types.Command;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.xeli.jpigpio.PigpioException;

/**
 * Class for the handling of generic i2c devices.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class I2CDeviceHandler extends BaseThingHandler {

    private final Logger logger = LoggerFactory.getLogger(I2CBusHandler.class);
    private final CommunicationHandler gpioHandler;

    private @Nullable Integer i2cDeviceHandler;

    /**
     * Instantiates a new generic I2C device.
     *
     * @param thing the thing
     */
    public I2CDeviceHandler(final Thing thing, final CommunicationHandler gpioHandler) {
        super(thing);
        this.gpioHandler = gpioHandler;
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
    public void initialize() {
        final I2CBusHandler busHandler = getBusHandler();
        if (gpioHandler.isConnected() && (busHandler != null)) {
            final I2CDeviceConfiguration config = getConfiguration();
            final int busId = busHandler.getConfiguration().getBusId();
            if (gpioHandler.i2cIsDeviceAvailable(busId, config.getAddress())) {
                try {
                    int i2cDeviceHandler = gpioHandler.i2cOpen(busId, config.getAddress());
                    if (configure(i2cDeviceHandler)) {
                        this.i2cDeviceHandler = i2cDeviceHandler;
                        updateStatus(ThingStatus.ONLINE, ThingStatusDetail.NONE, "I2C device is initialized.");
                    } else {
                        try {
                            gpioHandler.i2cClose(i2cDeviceHandler);
                        } catch (PigpioException ignored) {

                        }
                        this.i2cDeviceHandler = null;
                        final String message = String.format("Can't configure device %s.", config.getAddressAsHex());
                        updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.CONFIGURATION_ERROR, message);
                    }
                } catch (PigpioException exception) {
                    this.i2cDeviceHandler = null;
                    final String message = String.format("Can't open bus %d: %s.", busId, "Bad I2C bus");
                    switch (exception.getErrorCode()) {
                        case PI_BAD_I2C_ADDR, PI_BAD_I2C_BUS, PI_BAD_FLAGS, PI_NO_HANDLE ->
                            updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.CONFIGURATION_ERROR, message);
                        case PI_I2C_OPEN_FAILED ->
                            updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.COMMUNICATION_ERROR, message);
                        default -> updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.NONE, message);
                    }
                }
            } else {
                this.i2cDeviceHandler = null;
                final String message = String.format("Can't open device %s: Bad I2C address.",
                        config.getAddressAsHex());
                updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.CONFIGURATION_ERROR, message);
            }
        } else {
            if (gpioHandler.isConnected()) {
                final String message = "Can't initialize: Wrong bus handler configured.";
                updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.CONFIGURATION_ERROR, message);
            } else {
                updateStatus(ThingStatus.UNKNOWN, ThingStatusDetail.NOT_YET_READY, "Wait for I2C bus.");
                scheduler.schedule(this::initialize, 5, TimeUnit.SECONDS);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void dispose() {
        final Integer i2cDeviceHandler = this.i2cDeviceHandler;
        if (i2cDeviceHandler != null) {
            try {
                gpioHandler.i2cClose(i2cDeviceHandler);
            } catch (PigpioException exception) {
                logger.warn("Unable to close I2C device.");
            }
            this.i2cDeviceHandler = null;
        }
        super.dispose();
    }

    public I2CDeviceConfiguration getConfiguration() {
        return getConfigAs(I2CDeviceConfiguration.class);
    }

    public @Nullable I2CBusHandler getBusHandler() {
        final Bridge bridge = getBridge();
        if (bridge != null) {
            final BridgeHandler handler = bridge.getHandler();
            if (handler instanceof I2CBusHandler) {
                return (I2CBusHandler) handler;
            }
        }
        return null;
    }

    protected <T> T getConfigAs(final Channel channel, Class<T> configurationClass) {
        return channel.getConfiguration().as(configurationClass);
    }

    protected <T> @Nullable T getConfigAs(final ChannelUID channelUID, Class<T> configurationClass) {
        final Channel channel = getThing().getChannel(channelUID);
        return channel != null ? getConfigAs(channel, configurationClass) : null;
    }

    protected boolean configure(final int handle) {
        return true;
    }

    protected boolean read(int handle, int register, final byte[] data, int offset, int length) {
        try {
            final byte[] buffer = gpioHandler.i2cReadI2CBlockData(handle, register, length);
            if (buffer.length >= length) {
                System.arraycopy(buffer, 0, data, offset, length);
                return true;
            }
        } catch (PigpioException exception) {
            logger.warn("Can't read from device {}.", getConfiguration().getAddressAsHex());
        }
        return false;
    }

    protected boolean read(final int register, final byte[] data) {
        return read(register, data, 0, data.length);
    }

    protected boolean read(int register, final byte[] data, int offset, int length) {
        final Integer i2cDeviceHandler = this.i2cDeviceHandler;
        if (i2cDeviceHandler != null) {
            return read(i2cDeviceHandler, register, data, offset, length);
        } else {
            logger.warn("Can't read from device {}.", getConfiguration().getAddressAsHex());
            return false;
        }
    }

    protected boolean write(int handle, int register, final byte[] data, int offset, int length) {
        try {
            final byte[] buffer = new byte[length];
            System.arraycopy(data, offset, buffer, 0, length);
            gpioHandler.i2cWriteI2CBlockData(handle, register, buffer, length);
            return true;
        } catch (PigpioException exception) {
            logger.warn("Can't write to device {}.", getConfiguration().getAddressAsHex());
        }
        return false;
    }

    protected boolean write(final int register, final byte[] data) {
        return write(register, data, 0, data.length);
    }

    protected boolean write(int register, final byte[] data, int offset, int length) {
        final Integer i2cDeviceHandler = this.i2cDeviceHandler;
        if (i2cDeviceHandler != null) {
            return write(i2cDeviceHandler, register, data, offset, length);
        } else {
            logger.warn("Can't write to device {}.", getConfiguration().getAddressAsHex());
            return false;
        }
    }
}
