package eu.xeli.jpigpio;

import java.io.IOException;

public class PiGpioSocketI2C extends PigpioSocket {

    private boolean isConnected;
    private final int CMD_HWVER = 17; // 17 0 0 0 -
    private final int CMD_PIGPV = 26; // 26 0 0 0 -

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
}
