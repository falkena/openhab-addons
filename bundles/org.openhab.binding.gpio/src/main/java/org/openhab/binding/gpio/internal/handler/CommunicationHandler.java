/**
 * Copyright (c) 2010-2023 Alexander Falkenstern
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
package org.openhab.binding.gpio.internal.handler;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;

import eu.xeli.jpigpio.GPIOListener;
import eu.xeli.jpigpio.JPigpio;
import eu.xeli.jpigpio.PiGpioSocketI2C;
import eu.xeli.jpigpio.PigpioException;

@NonNullByDefault
public class CommunicationHandler {
    private @Nullable PiGpioSocketI2C remote;

    private String host = "Unknown";
    private Integer port = -1;

    public enum PinMode {
        INPUT,
        OUTPUT
    }

    public enum ResistorPullMode {
        OFF,
        PULL_UP,
        PULL_DOWN
    }

    public CommunicationHandler() {
    }

    public CommunicationHandler(final String host, int port) {
        this.host = host;
        this.port = port;
        connect(host, port);
    }

    public boolean connect(final String host, int port) {
        PiGpioSocketI2C remote = this.remote;
        if (remote == null) {
            try {
                this.host = host;
                this.port = port;
                remote = new PiGpioSocketI2C(host, port);
                this.remote = remote;
                return remote.isConnected();
            } catch (PigpioException exception) {
                return false;
            }
        } else {
            return remote.connect(host, port);
        }
    }

    public boolean disconnect() {
        this.host = "Unknown";
        this.port = -1;
        final PiGpioSocketI2C remote = this.remote;
        return ((remote != null) && remote.disconect());
    }

    public boolean isConnected() {
        final PiGpioSocketI2C remote = this.remote;
        return ((remote != null) && remote.isConnected());
    }

    public String getHost() {
        return host;
    }

    public Integer getPort() {
        return port;
    }

    public int getHardwareRevision() throws PigpioException {
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            return remote.gpioGetHardwareRevision();
        } else {
            throw new PigpioException("No remote available");
        }
    }

    public int getLibraryVersion() throws PigpioException {
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            return remote.gpioGetLibraryVersion();
        } else {
            throw new PigpioException("No remote available");
        }
    }

    public PinMode getPinMode(int pin) throws PigpioException {
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            return switch (remote.gpioGetMode(pin)) {
                case JPigpio.PI_INPUT -> PinMode.INPUT;
                case JPigpio.PI_OUTPUT -> PinMode.OUTPUT;
                default -> throw new PigpioException("Invalid pin mode");
            };
        } else {
            throw new PigpioException("No remote available");
        }
    }

    public void setPinMode(int pin, PinMode mode) throws PigpioException {
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            switch (mode) {
                case INPUT -> remote.gpioSetMode(pin, JPigpio.PI_INPUT);
                case OUTPUT -> remote.gpioSetMode(pin, JPigpio.PI_OUTPUT);
                default -> throw new PigpioException("Invalid pin mode");
            }
        } else {
            throw new PigpioException("No remote available.");
        }
    }

    public void setResistorMode(int pin, ResistorPullMode mode) throws PigpioException {
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            switch (mode) {
                case OFF -> remote.gpioSetPullUpDown(pin, JPigpio.PI_PUD_OFF);
                case PULL_DOWN -> remote.gpioSetPullUpDown(pin, JPigpio.PI_PUD_DOWN);
                case PULL_UP -> remote.gpioSetPullUpDown(pin, JPigpio.PI_PUD_UP);
                default -> throw new PigpioException("Invalid resistor mode");
            }
        } else {
            throw new PigpioException("No remote available");
        }
    }

    public boolean read(int pin) throws PigpioException {
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            return remote.gpioRead(pin);
        } else {
            throw new PigpioException("No remote available");
        }
    }

    public void write(int pin, boolean value) throws PigpioException {
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            remote.gpioWrite(pin, value);
        } else {
            throw new PigpioException("No remote available");
        }
    }

    public void trigger(int gpio, long pulse, boolean level) throws PigpioException {
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            remote.gpioTrigger(gpio, pulse, level);
        } else {
            throw new PigpioException("No remote available");
        }
    }

    public void debounce(int gpio, int steady) throws PigpioException {
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            remote.gpioGlitchFilter(gpio, 1000 * steady);
        } else {
            throw new PigpioException("No remote available");
        }
    }

    public void addListener(GPIOListener listener) throws PigpioException {
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            remote.addCallback(listener);
        } else {
            throw new PigpioException("No remote available");
        }
    }

    public void removeListener(GPIOListener listener) throws PigpioException {
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            remote.removeCallback(listener);
        } else {
            throw new PigpioException("No remote available");
        }
    }

    public int i2cOpen(int bus, int address) throws PigpioException {
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            return remote.i2cOpen(bus, address);
        } else {
            throw new PigpioException("No remote available");
        }
    }

    public void i2cClose(int handle) throws PigpioException {
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            remote.i2cClose(handle);
        } else {
            throw new PigpioException("No remote available");
        }
    }

    public int i2cReadDevice(int handle, final byte[] data) throws PigpioException {
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            return remote.i2cReadDevice(handle, data);
        } else {
            throw new PigpioException("No remote available");
        }
    }

    public void i2cWriteDevice(int handle, final byte[] data) throws PigpioException {
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            remote.i2cWriteDevice(handle, data);
        } else {
            throw new PigpioException("No remote available");
        }
    }

    public boolean i2cIsBusAvailable(int bus) {
        boolean result = false;
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            try {
                i2cClose(i2cOpen(bus, 0x00));
                result = true;
            } catch (PigpioException ignored) {
            }
        }
        return result;
    }
}
