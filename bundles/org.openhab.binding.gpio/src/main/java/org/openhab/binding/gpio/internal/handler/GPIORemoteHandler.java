/**
 * Copyright (c) 2010-2023 Contributors to the openHAB project
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
package org.openhab.binding.gpio.internal.handler;

import static org.openhab.binding.gpio.internal.GPIOBindingConstants.CHANNEL_TYPE_DIGITAL_INPUT;
import static org.openhab.binding.gpio.internal.GPIOBindingConstants.CHANNEL_TYPE_DIGITAL_OUTPUT;
import static org.openhab.core.thing.Thing.PROPERTY_FIRMWARE_VERSION;
import static org.openhab.core.thing.Thing.PROPERTY_HARDWARE_VERSION;
import static org.openhab.core.thing.Thing.PROPERTY_MODEL_ID;
import static org.openhab.core.thing.ThingStatusDetail.COMMUNICATION_ERROR;
import static org.openhab.core.thing.ThingStatusDetail.CONFIGURATION_ERROR;

import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.binding.gpio.internal.RasPiHardwareVersion;
import org.openhab.binding.gpio.internal.configuration.GPIOConfiguration;
import org.openhab.binding.gpio.internal.configuration.GPIOInputConfiguration;
import org.openhab.binding.gpio.internal.configuration.GPIOOutputConfiguration;
import org.openhab.binding.gpio.internal.configuration.GPIORemoteConfiguration;
import org.openhab.core.library.types.OnOffType;
import org.openhab.core.library.types.OpenClosedType;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.Channel;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusDetail;
import org.openhab.core.thing.binding.BaseBridgeHandler;
import org.openhab.core.thing.type.ChannelTypeUID;
import org.openhab.core.types.Command;
import org.openhab.core.types.RefreshType;
import org.openhab.core.types.UnDefType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.xeli.jpigpio.GPIOListener;
import eu.xeli.jpigpio.JPigpio;
import eu.xeli.jpigpio.PiGpioSocketI2C;
import eu.xeli.jpigpio.PigpioException;

/**
 * Remote pigpio Handler
 *
 * This bridge is used to control remote pigpio instances.
 *
 * @author Nils Bauer - Initial contribution
 * @author Jan N. Klug - Channel redesign
 */
@NonNullByDefault
public class GPIORemoteHandler extends BaseBridgeHandler {

    private final Logger logger = LoggerFactory.getLogger(GPIORemoteHandler.class);

    private class InterruptCallback extends GPIOListener {
        final Boolean isActiveHigh;
        final ChannelUID channelUID;

        public InterruptCallback(int gpio, int edge, ChannelUID channelUID, Boolean isActiveHigh) {
            super(gpio, edge);
            this.isActiveHigh = isActiveHigh;
            this.channelUID = channelUID;
        }

        @Override
        public void alert(int gpio, int level, long tick) {
            final Channel channel = thing.getChannel(channelUID);
            if (channel != null) {
                final ChannelTypeUID type = channel.getChannelTypeUID();
                if (CHANNEL_TYPE_DIGITAL_INPUT.equals(type)) {
                    updateState(channelUID, switch (level) {
                        case JPigpio.PI_ON -> isActiveHigh ? OpenClosedType.CLOSED : OpenClosedType.OPEN;
                        case JPigpio.PI_OFF -> isActiveHigh ? OpenClosedType.OPEN : OpenClosedType.CLOSED;
                        default -> UnDefType.UNDEF;
                    });
                } else if (CHANNEL_TYPE_DIGITAL_OUTPUT.equals(type)) {
                    updateState(channelUID, switch (level) {
                        case JPigpio.PI_ON -> isActiveHigh ? OnOffType.ON : OnOffType.OFF;
                        case JPigpio.PI_OFF -> isActiveHigh ? OnOffType.OFF : OnOffType.ON;
                        default -> UnDefType.UNDEF;
                    });
                } else {
                    updateState(channelUID, UnDefType.UNDEF);
                }
            } else {
                final String host = getConfiguration().getHost();
                logger.debug("Invalid channel {} on host {} found.", channelUID, host);
            }
        }
    }

    private final Set<InterruptCallback> callbacks = ConcurrentHashMap.newKeySet();

    private @Nullable PiGpioSocketI2C gpio = null;

    /**
     * Instantiates a new pigpio remote bridge handler.
     *
     * @param bridge the thing
     */
    public GPIORemoteHandler(Bridge bridge) {
        super(bridge);
    }

    @Override
    public void handleCommand(ChannelUID channelUID, Command command) {
        if (ThingStatus.ONLINE != thing.getStatus()) {
            return;
        }

        final PiGpioSocketI2C gpio = this.gpio;
        final Channel channel = thing.getChannel(channelUID);
        if ((channel != null) && (gpio != null)) {
            final ChannelTypeUID type = channel.getChannelTypeUID();
            final GPIOConfiguration config = channel.getConfiguration().as(GPIOConfiguration.class);
            if (command instanceof OpenClosedType) {
                final String host = getConfiguration().getHost();
                logger.debug("Received invalid command for channel {} on host {}.", channelUID, host);
            } else if (command instanceof OnOffType) {
                if (CHANNEL_TYPE_DIGITAL_OUTPUT.equals(type)) {
                    try {
                        if (config.isActiveOnHigh()) {
                            gpio.gpioWrite(config.getPin(), OnOffType.ON.equals(command));
                        } else {
                            gpio.gpioWrite(config.getPin(), OnOffType.OFF.equals(command));
                        }
                    } catch (PigpioException exception) {
                        updateState(channelUID, UnDefType.UNDEF);
                        final String host = getConfiguration().getHost();
                        logger.warn("Unable to write gpio state for channel {} on host {}.", channelUID, host);
                    }
                } else {
                    final String host = getConfiguration().getHost();
                    logger.debug("Received invalid command for channel {} on host {}.", channelUID, host);
                }
            } else if (command instanceof RefreshType) {
                if (CHANNEL_TYPE_DIGITAL_INPUT.equals(type)) {
                    try {
                        final Boolean isActiveHigh = config.isActiveOnHigh();
                        if (gpio.gpioRead(config.getPin())) {
                            updateState(channelUID, isActiveHigh ? OpenClosedType.CLOSED : OpenClosedType.OPEN);
                        } else {
                            updateState(channelUID, isActiveHigh ? OpenClosedType.OPEN : OpenClosedType.CLOSED);
                        }
                    } catch (PigpioException exception) {
                        updateState(channelUID, UnDefType.UNDEF);
                        final String host = getConfiguration().getHost();
                        logger.warn("Unable to read gpio state for channel {} on host {}.", channelUID, host);
                    }
                } else if (CHANNEL_TYPE_DIGITAL_OUTPUT.equals(type)) {
                    try {
                        final Boolean isActiveHigh = config.isActiveOnHigh();
                        if (gpio.gpioRead(config.getPin())) {
                            updateState(channelUID, isActiveHigh ? OnOffType.ON : OnOffType.OFF);
                        } else {
                            updateState(channelUID, isActiveHigh ? OnOffType.OFF : OnOffType.ON);
                        }
                    } catch (PigpioException exception) {
                        updateState(channelUID, UnDefType.UNDEF);
                        final String host = getConfiguration().getHost();
                        logger.warn("Unable to read gpio state for channel {} on host {}.", channelUID, host);
                    }
                } else {
                    final String host = getConfiguration().getHost();
                    logger.debug("Received invalid command for channel {} on host {}.", channelUID, host);
                }
            } else {
                final String host = getConfiguration().getHost();
                logger.debug("Not supported channel {} on host {} found.", channelUID, host);
            }
        } else {
            final String host = getConfiguration().getHost();
            logger.debug("Invalid channel {} on host {} found.", channelUID, host);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public synchronized void initialize() {
        final GPIORemoteConfiguration configuration = getConfiguration();
        if (gpio == null) {
            try {
                gpio = new PiGpioSocketI2C(configuration.getHost(), configuration.getPort());
            } catch (PigpioException exception) {
                try (Socket ignored = new Socket(configuration.getHost(), configuration.getPort())) {
                    updateStatus(ThingStatus.OFFLINE, COMMUNICATION_ERROR, exception.getLocalizedMessage());
                } catch (IllegalArgumentException | UnknownHostException reason) {
                    updateStatus(ThingStatus.OFFLINE, CONFIGURATION_ERROR, reason.getLocalizedMessage());
                } catch (IOException reason) {
                    updateStatus(ThingStatus.OFFLINE, COMMUNICATION_ERROR, reason.getLocalizedMessage());
                }
            }
        }

        final PiGpioSocketI2C gpio = this.gpio;
        if ((gpio != null) && gpio.connect(configuration.getHost(), configuration.getPort())) {
            updateStatus(ThingStatus.ONLINE, ThingStatusDetail.NONE, "GPIO remote is initialized.");
        } else {
            try (Socket ignored = new Socket(configuration.getHost(), configuration.getPort())) {
                updateStatus(ThingStatus.OFFLINE, COMMUNICATION_ERROR);
            } catch (IllegalArgumentException | UnknownHostException reason) {
                updateStatus(ThingStatus.OFFLINE, CONFIGURATION_ERROR, reason.getLocalizedMessage());
            } catch (IOException reason) {
                updateStatus(ThingStatus.OFFLINE, COMMUNICATION_ERROR, reason.getLocalizedMessage());
            }
        }

        if ((gpio != null) && (ThingStatus.ONLINE == getThing().getStatus())) {
            try {
                final int revision = gpio.getHardwareRevision();
                final RasPiHardwareVersion.Hardware hardware = RasPiHardwareVersion.getHardwareVersion(revision);
                thing.setProperty(PROPERTY_MODEL_ID, hardware.model());
                thing.setProperty(PROPERTY_HARDWARE_VERSION, hardware.revision());
            } catch (PigpioException exception) {
                thing.setProperty(PROPERTY_MODEL_ID, RasPiHardwareVersion.UNKNOWN.model());
                thing.setProperty(PROPERTY_HARDWARE_VERSION, RasPiHardwareVersion.UNKNOWN.revision());
            }
            try {
                final int version = gpio.gpioGetLibraryVersion();
                thing.setProperty(PROPERTY_FIRMWARE_VERSION, String.valueOf(version));
            } catch (PigpioException exception) {
                thing.setProperty(PROPERTY_FIRMWARE_VERSION, "Unknown");
            }
            getThing().getChannels().forEach(channel -> {
                final ChannelUID channelUID = channel.getUID();
                final ChannelTypeUID type = channel.getChannelTypeUID();
                if (CHANNEL_TYPE_DIGITAL_INPUT.equals(type)) {
                    final GPIOInputConfiguration config = channel.getConfiguration().as(GPIOInputConfiguration.class);
                    try {
                        gpio.gpioSetMode(config.getPin(), JPigpio.PI_INPUT);
                        try {
                            final Integer pullupdown = switch (config.getPullUpDownConfig()) {
                                case DOWN -> JPigpio.PI_PUD_DOWN;
                                case OFF -> JPigpio.PI_PUD_OFF;
                                case UP -> JPigpio.PI_PUD_UP;
                            };
                            gpio.gpioSetPullUpDown(config.getPin(), pullupdown);
                        } catch (PigpioException exception) {
                            logger.warn("Unable to setup resistor mode on channel {}.", channelUID);
                        }
                        try {
                            gpio.gpioGlitchFilter(config.getPin(), 1000 * config.debounce);
                        } catch (PigpioException exception) {
                            logger.warn("Unable to setup debounce time on channel {}.", channelUID);
                        }
                        try {
                            // PI_RISING_EDGE, PI_FALLING_EDGE, PI_EITHER_EDGE
                            final InterruptCallback callback = new InterruptCallback(config.getPin(),
                                    JPigpio.PI_EITHER_EDGE, channelUID, config.isActiveOnHigh());
                            gpio.addCallback(callback);
                            callbacks.add(callback);
                        } catch (PigpioException exception) {
                            logger.warn("Unable to register callback on channel {}.", channelUID);
                        }
                        handleCommand(channelUID, RefreshType.REFRESH);
                    } catch (PigpioException exception) {
                        final String message = String.format("Unable to set mode for channel %s.", channelUID);
                        updateStatus(ThingStatus.OFFLINE, CONFIGURATION_ERROR, message);
                    }
                } else if (CHANNEL_TYPE_DIGITAL_OUTPUT.equals(type)) {
                    final GPIOOutputConfiguration config = channel.getConfiguration().as(GPIOOutputConfiguration.class);
                    try {
                        gpio.gpioSetMode(config.getPin(), JPigpio.PI_OUTPUT);
                        try {
                            // PI_RISING_EDGE, PI_FALLING_EDGE, PI_EITHER_EDGE
                            final InterruptCallback callback = new InterruptCallback(config.getPin(),
                                    JPigpio.PI_EITHER_EDGE, channelUID, config.isActiveOnHigh());
                            gpio.addCallback(callback);
                            callbacks.add(callback);
                        } catch (PigpioException exception) {
                            logger.warn("Unable to register callback on channel {}.", channelUID);
                        }
                        handleCommand(channelUID, RefreshType.REFRESH);
                    } catch (PigpioException exception) {
                        final String message = String.format("Unable to set mode for channel %s.", channelUID);
                        updateStatus(ThingStatus.OFFLINE, CONFIGURATION_ERROR, message);
                    }
                }
            });
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public synchronized void dispose() {
        final PiGpioSocketI2C gpio = this.gpio;
        if (gpio != null) {
            callbacks.forEach((callback) -> {
                try {
                    gpio.removeCallback(callback);
                } catch (PigpioException exception) {
                    logger.warn("Unable to unregister callback.");
                }
            });
            gpio.disconect();
        }
        callbacks.clear();
        super.dispose();
    }

    public GPIORemoteConfiguration getConfiguration() {
        return getConfigAs(GPIORemoteConfiguration.class);
    }
}
