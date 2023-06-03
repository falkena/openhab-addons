/**
 * Copyright (c) 2010-2022 Alexander Falkenstern
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information.
 *
 * This program and the accompanying materials are made available
 * under the terms of the GNU General Public License v3.0 which is
 * available at https://www.gnu.org/licenses/gpl-3.0-standalone.html
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */
package org.openhab.binding.gpio.internal.handler.microchip;

import static eu.xeli.jpigpio.JPigpio.PI_EITHER_EDGE;
import static eu.xeli.jpigpio.JPigpio.PI_OFF;
import static eu.xeli.jpigpio.JPigpio.PI_ON;
import static java.util.concurrent.TimeUnit.MILLISECONDS;
import static org.openhab.binding.gpio.internal.GPIOBindingConstants.CHANNEL_TYPE_INTERRUPT;
import static org.openhab.binding.gpio.internal.GPIOBindingConstants.CHANNEL_TYPE_RESET;
import static org.openhab.binding.gpio.internal.GPIOBindingConstants.MCP23017_CHANNEL_TYPE_INPUT;
import static org.openhab.binding.gpio.internal.GPIOBindingConstants.MCP23017_CHANNEL_TYPE_OUTPUT;
import static org.openhab.binding.gpio.internal.GPIOUtilities.getBit;
import static org.openhab.binding.gpio.internal.GPIOUtilities.setBit;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledFuture;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.gpio.internal.configuration.InterruptPinConfiguration;
import org.openhab.binding.gpio.internal.configuration.ResetPinConfiguration;
import org.openhab.binding.gpio.internal.configuration.microchip.MCP23017Configuration;
import org.openhab.binding.gpio.internal.configuration.microchip.MCP23017Configuration.InterruptMode;
import org.openhab.binding.gpio.internal.configuration.microchip.MCP23017InputPinConfiguration;
import org.openhab.binding.gpio.internal.configuration.microchip.MCP23017InputPinConfiguration.InterruptConfig;
import org.openhab.binding.gpio.internal.configuration.microchip.MCP23017OutputPinConfiguration;
import org.openhab.binding.gpio.internal.configuration.microchip.MCP23017PinConfiguration;
import org.openhab.binding.gpio.internal.handler.CommunicationHandler;
import org.openhab.binding.gpio.internal.handler.I2CDeviceHandler;
import org.openhab.core.library.types.OnOffType;
import org.openhab.core.library.types.OpenClosedType;
import org.openhab.core.thing.Channel;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusDetail;
import org.openhab.core.thing.binding.ThingHandlerService;
import org.openhab.core.thing.type.ChannelTypeUID;
import org.openhab.core.types.Command;
import org.openhab.core.types.RefreshType;
import org.openhab.core.types.State;
import org.openhab.core.types.UnDefType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.xeli.jpigpio.GPIOListener;
import eu.xeli.jpigpio.PigpioException;

/**
 * The {@link MCP23017Handler} is responsible for handling commands, which are
 * sent to one of the channels.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public final class MCP23017Handler extends I2CDeviceHandler {

    private final Logger logger = LoggerFactory.getLogger(MCP23017Handler.class);

    private final CommunicationHandler gpioHandler;

    private @Nullable ScheduledFuture<?> inputReaderJob;
    private final Runnable inputReader = () -> {
        if (ThingStatus.ONLINE != getThing().getStatus()) {
            return;
        }

        final byte[] gpio = { 0x00, 0x00 };
        if (read(Registers.GPIO.getValue(), gpio)) {
            for (final Channel channel : getThing().getChannels()) {
                if (MCP23017_CHANNEL_TYPE_INPUT.equals(channel.getChannelTypeUID())) {
                    final MCP23017PinConfiguration config = getChannelConfigAs(channel, MCP23017PinConfiguration.class);
                    boolean value = getBit(gpio[config.getBank()], config.getPinAddress());
                    updateState(channel.getUID(), value ? OpenClosedType.CLOSED : OpenClosedType.OPEN);
                }
            }
        }
    };

    private @Nullable ScheduledFuture<?> outputReaderJob;
    private final Runnable outputReader = () -> {
        if (ThingStatus.ONLINE != getThing().getStatus()) {
            return;
        }

        final byte[] olat = { 0x00, 0x00 };
        if (read(Registers.OLAT.getValue(), olat)) {
            for (final Channel channel : getThing().getChannels()) {
                if (MCP23017_CHANNEL_TYPE_OUTPUT.equals(channel.getChannelTypeUID())) {
                    final MCP23017PinConfiguration config = getChannelConfigAs(channel, MCP23017PinConfiguration.class);
                    boolean value = getBit(olat[config.getBank()], config.getPinAddress());
                    updateState(channel.getUID(), value ? OnOffType.ON : OnOffType.OFF);
                }
            }
        }
    };

    private class InterruptListener extends GPIOListener {

        public InterruptListener(final InterruptPinConfiguration config) {
            super(config.getPin(), PI_EITHER_EDGE);
        }

        @Override
        public void alert(int gpio, int level, long tick) {
            final byte[] flags = { 0x00, 0x00 };
            final byte[] captured = { 0x00, 0x00 };
            if (read(Registers.INTF.getValue(), flags) && read(Registers.INTCAP.getValue(), captured)) {
                for (final Channel channel : getThing().getChannels()) {
                    if (MCP23017_CHANNEL_TYPE_INPUT.equals(channel.getChannelTypeUID())) {
                        final MCP23017InputPinConfiguration config = getChannelConfigAs(channel,
                                MCP23017InputPinConfiguration.class);
                        final boolean flag = getBit(flags[config.getBank()], config.getPinAddress());
                        final boolean value = getBit(captured[config.getBank()], config.getPinAddress());
                        switch (config.getInterruptConfig()) {
                            case HIGH -> {
                                if (flag && value) { // LOW-HIGH interrupt
                                    triggerChannel(channel.getUID(), OpenClosedType.CLOSED.toString());
                                }
                            }
                            case LOW -> {
                                if (flag && !value) { // HIGH-LOW interrupt
                                    triggerChannel(channel.getUID(), OpenClosedType.OPEN.toString());
                                }
                            }
                            case PREVIOUS -> { // LOW-HIGH/HIGH-LOW change
                                final State state = flag && value ? OpenClosedType.CLOSED : OpenClosedType.OPEN;
                                triggerChannel(channel.getUID(), state.toString());
                            }
                            default -> {
                            }
                        }
                    }
                }
            }
        }
    }

    private class ResetListener extends GPIOListener {

        final Boolean isActiveHigh;
        final ChannelUID channelUID;

        public ResetListener(final ResetPinConfiguration config, final ChannelUID channelUID) {
            super(config.getPin(), PI_EITHER_EDGE);
            this.isActiveHigh = config.isActiveOnHigh();
            this.channelUID = channelUID;
        }

        @Override
        public void alert(int gpio, int level, long tick) {
            if (((level == PI_ON) && isActiveHigh) || ((level == PI_OFF) && !isActiveHigh)) {
                stopReaderJobs();
                MCP23017Handler.super.dispose(); // Reset device only
                updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.NOT_YET_READY, "MCP23017 is resetting.");
            } else if (((level == PI_ON) && !isActiveHigh) || ((level == PI_OFF) && isActiveHigh)) {
                MCP23017Handler.super.initialize(); // Reset device only
                startReaderJobs();
            } else {
                logger.debug("Received wrong gpio state for channel {}.", channelUID);
            }
        }
    }

    private final Set<GPIOListener> listeners = ConcurrentHashMap.newKeySet();

    private enum Registers {
        IODIR(0x00),
        IPOL(0x02),
        GPINTEN(0x04),
        DEFVAL(0x06),
        INTCON(0x08),
        IOCON(0x0A),
        GPPU(0x0C),
        INTF(0x0E),
        INTCAP(0x10),
        GPIO(0x12),
        OLAT(0x14);

        private final int value;

        Registers(int value) {
            this.value = value;
        }

        public int getValue() {
            return value;
        }
    }

    /**
     * Instantiates a new MCP23017 I2C device.
     *
     * @param thing the thing
     */
    public MCP23017Handler(final Thing thing, final CommunicationHandler gpioHandler) {
        super(thing, gpioHandler);
        this.gpioHandler = gpioHandler;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void handleCommand(@NonNull ChannelUID channelUID, @NonNull Command command) {
        final Channel channel = getThing().getChannel(channelUID);
        if ((channel != null) && (command instanceof OnOffType)) {
            if (CHANNEL_TYPE_RESET.equals(channel.getChannelTypeUID())) {
                final ResetPinConfiguration config = getChannelConfigAs(channel, ResetPinConfiguration.class);
                if (config.isPinValid()) {
                    try {
                        gpioHandler.write(config.getPin(), config.isActiveOnHigh() == OnOffType.ON.equals(command));
                    } catch (PigpioException exception) {
                        updateState(channelUID, UnDefType.UNDEF);
                        final String address = getConfigAs(MCP23017Configuration.class).getAddressAsHex();
                        logger.warn("Failed to set channel {} for device {}.", channelUID, address);
                    }
                }
            }
        }

        if (ThingStatus.ONLINE != getThing().getStatus()) {
            return;
        }

        final String address = getConfigAs(MCP23017Configuration.class).getAddressAsHex();
        if (channel != null) {
            final ChannelTypeUID type = channel.getChannelTypeUID();
            if (command instanceof OpenClosedType) {
                if (CHANNEL_TYPE_INTERRUPT.equals(type)) {
                    logger.debug("Invalid command on input channel {} for device {} received.", channelUID, address);
                } else if (MCP23017_CHANNEL_TYPE_INPUT.equals(type)) {
                    logger.debug("Invalid command on input channel {} for device {} received.", channelUID, address);
                } else {
                    logger.debug("Invalid channel {} for device {} found.", channelUID, address);
                }
            } else if (command instanceof OnOffType) {
                if (MCP23017_CHANNEL_TYPE_OUTPUT.equals(type)) {
                    final byte[] data = { 0x00 };
                    final MCP23017PinConfiguration config = getChannelConfigAs(channel, MCP23017PinConfiguration.class);
                    if (read(Registers.GPIO.getValue() + config.getBank(), data)) {
                        data[0] = setBit(data[0], config.getPinAddress(), OnOffType.ON.equals(command));
                        if (!write(Registers.GPIO.getValue() + config.getBank(), data)) {
                            updateState(channelUID, UnDefType.UNDEF);
                        }
                    }
                } else {
                    logger.debug("Invalid channel {} for device {} found.", channelUID, address);
                }
            } else if (command instanceof RefreshType) {
                if (CHANNEL_TYPE_INTERRUPT.equals(type)) {
                    State state = UnDefType.UNDEF;
                    final InterruptPinConfiguration config = getChannelConfigAs(channel,
                            InterruptPinConfiguration.class);
                    if (config.isPinValid()) {
                        try {
                            if (gpioHandler.read(config.getPin())) {
                                state = (config.isActiveOnHigh() ? OpenClosedType.OPEN : OpenClosedType.CLOSED);
                            } else {
                                state = (config.isActiveOnHigh() ? OpenClosedType.CLOSED : OpenClosedType.OPEN);
                            }
                        } catch (PigpioException exception) {
                            updateState(channelUID, UnDefType.UNDEF);
                            logger.warn("Unable to read gpio state for channel {} on host {}.", channelUID,
                                    gpioHandler.getHost());
                        }
                    }
                    updateState(channelUID, state);
                } else if (CHANNEL_TYPE_RESET.equals(type)) {
                    State state = UnDefType.UNDEF;
                    final ResetPinConfiguration config = getChannelConfigAs(channel, ResetPinConfiguration.class);
                    if (config.isPinValid()) {
                        try {
                            if (gpioHandler.read(config.getPin())) {
                                state = (config.isActiveOnHigh() ? OnOffType.ON : OnOffType.OFF);
                            } else {
                                state = (config.isActiveOnHigh() ? OnOffType.OFF : OnOffType.ON);
                            }
                        } catch (PigpioException exception) {
                            updateState(channelUID, UnDefType.UNDEF);
                            logger.warn("Unable to read gpio state for channel {} on host {}.", channelUID,
                                    gpioHandler.getHost());
                        }
                    }
                    updateState(channelUID, state);
                } else if (MCP23017_CHANNEL_TYPE_INPUT.equals(type)) {
                    final byte[] data = { 0x00 };
                    final MCP23017PinConfiguration config = getChannelConfigAs(channel, MCP23017PinConfiguration.class);
                    if (read(Registers.GPIO.getValue() + config.getBank(), data)) {
                        boolean value = getBit(data[0], config.getPinAddress());
                        updateState(channelUID, value ? OpenClosedType.CLOSED : OpenClosedType.OPEN);
                    }
                } else if (MCP23017_CHANNEL_TYPE_OUTPUT.equals(type)) {
                    final byte[] data = { 0x00 };
                    final MCP23017PinConfiguration config = getChannelConfigAs(channel, MCP23017PinConfiguration.class);
                    if (read(Registers.OLAT.getValue() + config.getBank(), data)) {
                        boolean value = getBit(data[0], config.getPinAddress());
                        updateState(channelUID, value ? OnOffType.ON : OnOffType.OFF);
                    }
                } else {
                    logger.debug("Invalid channel {} for device {} found.", channelUID, address);
                }
            } else {
                logger.debug("Invalid command on channel {} for device {} received.", channelUID, address);
            }
        } else {
            logger.debug("Invalid channel {} for device {} found.", channelUID, address);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void initialize() {
        super.initialize();
        if (ThingStatus.ONLINE == getThing().getStatus()) {
            final ChannelUID interruptChannelUID = new ChannelUID(getThing().getUID(), CHANNEL_TYPE_INTERRUPT.getId());
            final InterruptPinConfiguration interruptPinConfig = getChannelConfigAs(interruptChannelUID,
                    InterruptPinConfiguration.class);
            if ((interruptPinConfig != null) && interruptPinConfig.isPinValid()) {
                try {
                    gpioHandler.setPinMode(interruptPinConfig.getPin(), CommunicationHandler.PinMode.INPUT);
                    try {
                        gpioHandler.debounce(interruptPinConfig.getPin(), interruptPinConfig.debounce);
                    } catch (PigpioException ignored) {
                        final String address = getConfigAs(MCP23017Configuration.class).getAddressAsHex();
                        logger.warn("Unable to set debouncing for device {}.", address);
                    }
                    try {
                        final InterruptListener listener = new InterruptListener(interruptPinConfig);
                        gpioHandler.addListener(listener);
                        listeners.add(listener);
                    } catch (PigpioException exception) {
                        final String address = getConfigAs(MCP23017Configuration.class).getAddressAsHex();
                        logger.warn("Unable to register interrupt callback for device {}.", address);
                    }
                } catch (PigpioException exception) {
                    final String address = getConfigAs(MCP23017Configuration.class).getAddressAsHex();
                    logger.warn("Unable to configure channel {} for device {}.", interruptChannelUID, address);
                }
            }

            final ChannelUID resetChannelUID = new ChannelUID(getThing().getUID(), CHANNEL_TYPE_RESET.getId());
            final ResetPinConfiguration resetPinConfig = getChannelConfigAs(resetChannelUID,
                    ResetPinConfiguration.class);
            if ((resetPinConfig != null) && resetPinConfig.isPinValid()) {
                try {
                    gpioHandler.setPinMode(resetPinConfig.getPin(), CommunicationHandler.PinMode.OUTPUT);
                    try {
                        final ResetListener listener = new ResetListener(resetPinConfig, resetChannelUID);
                        gpioHandler.addListener(listener);
                        listeners.add(listener);
                    } catch (PigpioException exception) {
                        final String address = getConfigAs(MCP23017Configuration.class).getAddressAsHex();
                        logger.warn("Unable to register reset callback for {}.", address);
                    }
                } catch (PigpioException exception) {
                    final String address = getConfigAs(MCP23017Configuration.class).getAddressAsHex();
                    logger.warn("Unable to configure channel {} for device {}.", resetChannelUID, address);
                }
            }

            getThing().getChannels().forEach((channel) -> {
                handleCommand(channel.getUID(), RefreshType.REFRESH);
            });
            startReaderJobs();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void dispose() {
        stopReaderJobs();
        listeners.forEach((listener) -> {
            try {
                gpioHandler.removeListener(listener);
            } catch (PigpioException exception) {
                logger.warn("Unable to unregister callback.");
            }
        });
        listeners.clear();
        super.dispose();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void thingUpdated(@NonNull Thing thing) {
        super.thingUpdated(thing);
        getThing().getChannels().forEach((channel) -> handleCommand(channel.getUID(), RefreshType.REFRESH));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public @NonNull Collection<@NonNull Class<? extends @NonNull ThingHandlerService>> getServices() {
        return Collections.singleton(MCP23017PinProvider.class);
    }

    @Override
    protected boolean configure(final int handle) {
        if (super.configure(handle)) {
            final byte[] data = { 0x00, 0x00 };
            boolean success = read(handle, 0x05, data, 0, 1); // Assume IOCON.BANK = 1
            data[0] = setBit(data[0], 7, false); // Bit 7: Bank select
            success = success && write(handle, 0x05, data, 0, 1); // Now IOCON.BANK = 0

            // Reread chip configuration
            success = success && read(handle, Registers.IOCON.getValue(), data, 0, 1);

            final MCP23017Configuration config = getConfigAs(MCP23017Configuration.class);
            data[0] = setBit(data[0], 6, config.isInterruptMirrored()); // Bit 6: Mirror interrupt pins
            data[0] = setBit(data[0], 5, true); // Bit 5: Disable sequential operation mode
            data[0] = setBit(data[0], 4, false); // Bit 4: Enable slew rate
            data[0] = setBit(data[0], 3, false); // Bit 3: Not used on MCP23017

            final InterruptMode mode = config.getInterruptPinMode();
            data[0] = setBit(data[0], 2, InterruptMode.OPEN_DRAIN == mode); // Bit 2: Enable Open-Drain
            data[0] = setBit(data[0], 1, InterruptMode.HIGH == mode); // Bit 1: Enable Active-High

            success = success && write(handle, Registers.IOCON.getValue(), data, 0, 1);
            success = success && read(handle, Registers.IOCON.getValue() + 1, data, 1, 1);
            success = success && (data[0] == data[1]); // Register IOCONA and IOCONB shall be equal

            for (Channel channel : getThing().getChannels()) {
                final ChannelTypeUID type = channel.getChannelTypeUID();
                if (MCP23017_CHANNEL_TYPE_INPUT.equals(type)) {
                    final MCP23017InputPinConfiguration pin = getChannelConfigAs(channel,
                            MCP23017InputPinConfiguration.class);

                    if (success && read(handle, Registers.IODIR.getValue() + pin.getBank(), data, 0, 1)) {
                        data[0] = setBit(data[0], pin.getPinAddress(), true); // Input: true, Output: false
                        success = write(handle, Registers.IODIR.getValue() + pin.getBank(), data, 0, 1);
                    }

                    if (success && read(handle, Registers.IPOL.getValue() + pin.getBank(), data, 0, 1)) {
                        data[0] = setBit(data[0], pin.getPinAddress(), !pin.isActiveHigh());
                        success = write(handle, Registers.IPOL.getValue() + pin.getBank(), data, 0, 1);
                    }

                    final InterruptConfig interrupt = pin.getInterruptConfig();
                    if (success && read(handle, Registers.GPINTEN.getValue() + pin.getBank(), data, 0, 1)) {
                        data[0] = setBit(data[0], pin.getPinAddress(), interrupt != InterruptConfig.OFF);
                        success = write(handle, Registers.GPINTEN.getValue() + pin.getBank(), data, 0, 1);
                    }
                    if (success && read(handle, Registers.INTCON.getValue() + pin.getBank(), data, 0, 1)) {
                        data[0] = setBit(data[0], pin.getPinAddress(), interrupt != InterruptConfig.PREVIOUS);
                        success = write(handle, Registers.INTCON.getValue() + pin.getBank(), data, 0, 1);
                    }
                    if (success && read(handle, Registers.DEFVAL.getValue() + pin.getBank(), data, 0, 1)) {
                        data[0] = setBit(data[0], pin.getPinAddress(), interrupt == InterruptConfig.LOW);
                        success = write(handle, Registers.DEFVAL.getValue() + pin.getBank(), data, 0, 1);
                    }

                    if (success && read(handle, Registers.GPPU.getValue() + pin.getBank(), data, 0, 1)) {
                        data[0] = setBit(data[0], pin.getPinAddress(), pin.isPullUpEnabled());
                        success = write(handle, Registers.GPPU.getValue() + pin.getBank(), data, 0, 1);
                    }
                    if (!success) {
                        final String address = getConfigAs(MCP23017Configuration.class).getAddressAsHex();
                        logger.warn("Unable to configure channel {} for device {}.", channel.getUID(), address);
                    }
                } else if (MCP23017_CHANNEL_TYPE_OUTPUT.equals(type)) {
                    final MCP23017OutputPinConfiguration pin = getChannelConfigAs(channel,
                            MCP23017OutputPinConfiguration.class);

                    if (success && read(handle, Registers.IODIR.getValue() + pin.getBank(), data, 0, 1)) {
                        data[0] = setBit(data[0], pin.getPinAddress(), false); // Input: true, Output: false
                        success = write(handle, Registers.IODIR.getValue() + pin.getBank(), data, 0, 1);
                    }

                    if (success && read(handle, Registers.GPIO.getValue() + pin.getBank(), data, 0, 1)) {
                        data[0] = setBit(data[0], pin.getPinAddress(), false); // Input: true, Output: false
                        success = write(handle, Registers.GPIO.getValue() + pin.getBank(), data, 0, 1);
                    }
                    if (!success) {
                        final String address = config.getAddressAsHex();
                        logger.warn("Unable to configure channel {} for device {}.", channel.getUID(), address);
                    }
                }
            }

            return success;
        }
        return false;
    }

    private void startReaderJobs() {
        final MCP23017Configuration config = getConfigAs(MCP23017Configuration.class);
        final Integer interval = config.getRefreshInterval();

        final ScheduledFuture<?> inputJob = inputReaderJob;
        final ChannelUID interruptChannelUID = new ChannelUID(getThing().getUID(), CHANNEL_TYPE_INTERRUPT.getId());
        final InterruptPinConfiguration pin = getChannelConfigAs(interruptChannelUID, InterruptPinConfiguration.class);
        if (((inputJob == null) || inputJob.isCancelled()) && ((pin == null) || !pin.isPinValid())) {
            logger.info("Creating new input reader job for {} with interval {} ms.", config.getAddressAsHex(),
                    interval);
            inputReaderJob = scheduler.scheduleWithFixedDelay(inputReader, 1, interval, MILLISECONDS);
        }

        final ScheduledFuture<?> outputJob = outputReaderJob;
        if ((outputJob == null) || outputJob.isCancelled()) {
            logger.info("Creating new output reader job for {} with interval {} ms.", config.getAddressAsHex(),
                    interval);
            outputReaderJob = scheduler.scheduleWithFixedDelay(outputReader, 1, interval, MILLISECONDS);
        }
    }

    private void stopReaderJobs() {
        final MCP23017Configuration config = getConfigAs(MCP23017Configuration.class);

        final ScheduledFuture<?> inputJob = inputReaderJob;
        if ((inputJob != null) && !inputJob.isCancelled()) {
            inputReaderJob = null;
            inputJob.cancel(false);
            logger.info("Destroy input reader job for {}.", config.getAddressAsHex());
        }

        final ScheduledFuture<?> outputJob = outputReaderJob;
        if ((outputJob != null) && !outputJob.isCancelled()) {
            outputReaderJob = null;
            outputJob.cancel(false);
            logger.info("Destroy output reader job for {}.", config.getAddressAsHex());
        }
    }
}
