package eu.xeli.jpigpio;

import java.io.IOException;

public class PiGpioSocketI2C extends PigpioSocket {

    private boolean isConnected;
    private final int CMD_HWVER = 17; // 17 0 0 0 -
    private final int CMD_PIGPV = 26; // 26 0 0 0 -

    private final int CMD_I2CWQ = 58; // 58 handle bit 0 -
    private final int CMD_I2CRS = 59; // 59 handle 0 0 -
    private final int CMD_I2CWS = 60; // 60 handle byte 0 -
    private final int CMD_I2CRB = 61; // 61 handle register 0 -
    private final int CMD_I2CWB = 62; // 62 handle register 4 uint32_t byte
    private final int CMD_I2CRW = 63; // 63 handle register 0 -
    private final int CMD_I2CWW = 64; // 64 handle register 4 uint32_t word
    private final int CMD_I2CRK = 65; // 65 handle register 0 -
    private final int CMD_I2CWK = 66; // 66 handle register X uint8_t bvs[X]
    private final int CMD_I2CRI = 67; // 67 handle register 4 uint32_t num
    private final int CMD_I2CWI = 68; // 68 handle register X uint8_t bvs[X]
    private final int CMD_I2CPC = 69; // 69 handle register 4 uint32_t word
    private final int CMD_I2CPK = 70; // 70 handle register X uint8_t data[X]

    public PiGpioSocketI2C(String host, int port) throws PigpioException {
        super(host, port);
        isConnected = true;
    }

    public boolean connect(String host, int port) {
        if (!isConnected) {
            try {
                this.host = host;
                this.port = port;
                this.gpioInitialize();
                isConnected = true;
            } catch (PigpioException exception) {
                isConnected = false;
            }
        }
        return isConnected;
    }

    public boolean disconect() {
        if (isConnected) {
            try {
                this.gpioTerminate();
            } catch (PigpioException ignored) {
            } finally {
                isConnected = false;
            }
        }
        return isConnected;
    }

    public boolean isConnected() {
        return isConnected;
    }

    public int gpioGetHardwareRevision() throws PigpioException {
        try {
            int rc = slCmd.sendCmd(CMD_HWVER, 0, 0);
            if (rc < 0) {
                throw new PigpioException(rc);
            }
            return rc;
        } catch (IOException e) {
            throw new PigpioException("gpioGetHardwareRevision", e);
        }
    } // End of getHardwareRevision

    public int gpioGetLibraryVersion() throws PigpioException {
        try {
            int rc = slCmd.sendCmd(CMD_PIGPV, 0, 0);
            if (rc < 0) {
                throw new PigpioException(rc);
            }
            return rc;
        } catch (IOException e) {
            throw new PigpioException("gpioGetLibraryVersion", e);
        }
    } // End of gpioGetLibraryVersion

    public int i2cWriteBit(int handle, boolean bit) throws PigpioException {
        try {
            int rc = slCmd.sendCmd(CMD_I2CWQ, handle, bit ? 1 : 0);
            if (rc < 0) {
                throw new PigpioException(rc);
            } else if (rc > 0) {
                throw new PigpioException(PigpioException.PI_I2C_WRITE_FAILED);
            } else {
                return rc;
            }
        } catch (IOException e) {
            throw new PigpioException("i2cWriteQuick", e);
        }
    } // End of i2cWriteQuick

    public byte i2cReadByte(int handle) throws PigpioException {
        try {
            int rc = slCmd.sendCmd(CMD_I2CRS, handle, 0);
            if (rc < 0) {
                throw new PigpioException(rc);
            }
            return (byte) (rc & 0xFF);
        } catch (IOException e) {
            throw new PigpioException("i2cReadByte", e);
        }
    } // End of i2cReadByte

    public int i2cWriteByte(int handle, byte data) throws PigpioException {
        try {
            int rc = slCmd.sendCmd(CMD_I2CWS, handle, data & 0xFF);
            if (rc < 0) {
                throw new PigpioException(rc);
            } else if (rc > 0) {
                throw new PigpioException(PigpioException.PI_I2C_WRITE_FAILED);
            } else {
                return rc;
            }
        } catch (IOException e) {
            throw new PigpioException("i2cWriteByte", e);
        }
    } // End of i2cWriteByte

    public byte i2cReadByteData(int handle, int i2cRegister) throws PigpioException {
        try {
            int rc = slCmd.sendCmd(CMD_I2CRB, handle, i2cRegister);
            if (rc < 0) {
                throw new PigpioException(rc);
            }
            return (byte) (rc & 0xFF);
        } catch (IOException e) {
            throw new PigpioException("i2cReadByteData", e);
        }
    } // End of i2cReadByteData

    public int i2cWriteByteData(int handle, int i2cRegister, byte data) throws PigpioException {
        try {
            final byte[] buffer = { data, 0, 0, 0 };
            int rc = slCmd.sendCmd(CMD_I2CWB, handle, i2cRegister, buffer.length, buffer);
            if (rc < 0) {
                throw new PigpioException(rc);
            } else if (rc > 0) {
                throw new PigpioException(PigpioException.PI_I2C_WRITE_FAILED);
            } else {
                return rc;
            }
        } catch (IOException e) {
            throw new PigpioException("i2cWriteByteData", e);
        }
    } // End of i2cWriteByteData

    public short i2cReadWordData(int handle, int i2cRegister) throws PigpioException {
        try {
            int rc = slCmd.sendCmd(CMD_I2CRW, handle, i2cRegister);
            if (rc < 0) {
                throw new PigpioException(rc);
            }
            return (byte) (rc & 0xFFFF);
        } catch (IOException e) {
            throw new PigpioException("i2cReadWordData", e);
        }
    } // End of i2cReadWordData

    public int i2cWriteWordData(int handle, int i2cRegister, short data) throws PigpioException {
        try {
            final byte[] buffer = { (byte) (data & 0xFF), (byte) ((data >> 8) & 0xFF), 0, 0 };
            int rc = slCmd.sendCmd(CMD_I2CWW, handle, i2cRegister, buffer.length, buffer);
            if (rc < 0) {
                throw new PigpioException(rc);
            } else if (rc > 0) {
                throw new PigpioException(PigpioException.PI_I2C_WRITE_FAILED);
            } else {
                return rc;
            }
        } catch (IOException e) {
            throw new PigpioException("i2cWriteWordData", e);
        }
    } // End of i2cWriteWordData

    public byte[] i2cReadBlockData(int handle, int i2cRegister) throws PigpioException {
        try {
            int rc = slCmd.sendCmd(CMD_I2CRK, handle, i2cRegister);
            if (rc < 0) {
                throw new PigpioException(rc);
            }
            final byte[] result = new byte[rc];
            slCmd.readBytes(result);
            return result;
        } catch (IOException e) {
            throw new PigpioException("i2cReadBlockData", e);
        }
    } // End of i2cReadBlockData

    public int i2cWriteBlockData(int handle, int i2cRegister, byte[] data) throws PigpioException {
        try {
            int rc = slCmd.sendCmd(CMD_I2CWK, handle, i2cRegister, data.length, data);
            if (rc < 0) {
                throw new PigpioException(rc);
            } else if (rc > 0) {
                throw new PigpioException(PigpioException.PI_I2C_WRITE_FAILED);
            } else {
                return rc;
            }
        } catch (IOException e) {
            throw new PigpioException("i2cWriteBlockData", e);
        }
    } // End of i2cWriteBlockData

    public byte[] i2cReadI2CBlockData(int handle, int i2cRegister, int count) throws PigpioException {
        try {
            final byte[] buffer = { (byte) (count & 0xFF), (byte) ((count >> 8) & 0xFF), (byte) ((count >> 16) & 0xFF),
                    (byte) ((count >> 24) & 0xFF) };
            int rc = slCmd.sendCmd(CMD_I2CRI, handle, i2cRegister, buffer.length, buffer);
            if (rc < 0) {
                throw new PigpioException(rc);
            } else if (rc != count) {
                throw new PigpioException(PigpioException.PI_I2C_READ_FAILED);
            } else {
                final byte[] result = new byte[rc];
                slCmd.readBytes(result);
                return result;
            }
        } catch (IOException e) {
            throw new PigpioException("i2cReadI2CBlockData", e);
        }
    } // End of i2cReadI2CBlockData

    public int i2cWriteI2CBlockData(int handle, int i2cRegister, byte[] data, int count) throws PigpioException {
        try {
            if (count > data.length) {
                throw new PigpioException(PigpioException.PI_BAD_PARAM);
            }
            int rc = slCmd.sendCmd(CMD_I2CWI, handle, i2cRegister, count, data);
            if (rc < 0) {
                throw new PigpioException(rc);
            } else if (rc > 0) {
                throw new PigpioException(PigpioException.PI_I2C_WRITE_FAILED);
            } else {
                return rc;
            }
        } catch (IOException e) {
            throw new PigpioException("i2cWriteI2CBlockData", e);
        }
    } // End of i2cWriteI2CBlockData

    public short i2cProcessCall(int handle, int i2cRegister, short data) throws PigpioException {
        try {
            final byte[] buffer = { (byte) (data & 0xFF), (byte) ((data >> 8) & 0xFF), 0, 0 };
            int rc = slCmd.sendCmd(CMD_I2CPC, handle, i2cRegister, buffer.length, buffer);
            if (rc < 0) {
                throw new PigpioException(rc);
            }
            return (short) (rc & 0xFFFF);
        } catch (IOException e) {
            throw new PigpioException("i2cProcessCall", e);
        }
    } // End of i2cProcessCall

    public byte[] i2cBlockProcessCall(int handle, int i2cRegister, byte[] data) throws PigpioException {
        try {
            int rc = slCmd.sendCmd(CMD_I2CPK, handle, i2cRegister, data.length, data);
            if (rc < 0) {
                throw new PigpioException(rc);
            }
            final byte[] result = new byte[rc];
            slCmd.readBytes(result);
            return result;
        } catch (IOException e) {
            throw new PigpioException("i2cBlockProcessCall", e);
        }
    } // End of i2cBlockProcessCall
}
