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

import static eu.xeli.jpigpio.PigpioException.PI_I2C_WRITE_FAILED;

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

    public byte[] i2cReadI2CBlockData(int handle, int register, int count) throws PigpioException {
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            return remote.i2cReadI2CBlockData(handle, register, count);
        } else {
            throw new PigpioException("No remote available");
        }
    }

    public void i2cWriteI2CBlockData(int handle, int register, final byte[] data, int count) throws PigpioException {
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            int rc = remote.i2cWriteI2CBlockData(handle, register, data, count);
            if (rc != 0) {
                final PigpioException exception = new PigpioException(PI_I2C_WRITE_FAILED);
                throw new PigpioException(exception.getMessage());
            }
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

    // See https://sources.debian.org/src/i2c-tools/4.3-2/tools/i2cdetect.c/
    public boolean i2cIsDeviceAvailable(int bus, int address) {
        boolean result = false;
        final PiGpioSocketI2C remote = this.remote;
        if (remote != null) {
            try {
                final int handle = i2cOpen(bus, address);
                if (((0x30 <= address) && (address <= 0x37)) || ((0x50 <= address) && (address <= 0x5F))) {
                    try {
                        // This is known to lock SMBus on various write-only chips (mainly clock chips)
                        result = (remote.i2cReadByte(handle) >= 0);
                    } catch (PigpioException ignored) {
                    }
                } else {
                    try {
                        // This is known to corrupt the Atmel AT24RF08 EEPROM
                        result = (remote.i2cWriteByte(handle, (byte) 0) >= 0);
                    } catch (PigpioException ignored) {
                    }
                }
                try {
                    i2cClose(handle);
                } catch (PigpioException ignored) {
                }
            } catch (PigpioException ignored) {
            }
        }
        return result;
    }
}
