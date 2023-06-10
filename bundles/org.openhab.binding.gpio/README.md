# GPIO Binding

This binding adds GPIO support via the pigpio daemon to openhab.
It requires the pigpio (<http://abyz.me.uk/rpi/pigpio/>) to be running on the pi that should be controlled.

## Supported Things

### remote

This bridge represents a remote pigpio instance running as daemon on a raspberry pi.

### i2c-bus

This bridge represents a I2C bus instance on a raspberry pi.

### i2c-device

This thing represents general I2C device.

### i2c-mcp23017

This thing represents MCP23017 I2C device.

## Thing Configuration

### Pigpio Remote  (`remote`)

On a raspberry (or a compatible device) you have to install pigpio:

```shell
sudo apt-get install pigpiod
sudo raspi-config 
```

-> Interfacing Options --> Remote GPIO --> YES --> OK --> Finish

Note: if you are setting this up on a Raspberry Pi without `raspi-config` you can create the service config file manually:

```shell
sudo mkdir -p /etc/systemd/system/pigpiod.service.d/
sudo nano /etc/systemd/system/pigpiod.service.d/public.conf
```

```text
[Service]
ExecStart=
ExecStart=/usr/bin/pigpiod
```

```shell
sudo systemctl daemon-reload
```

Now that Remote GPIO is enabled, get the daemon going (even if installed with apt-get):

```shell
sudo systemctl enable pigpiod 
sudo systemctl start pigpiod
```

In openHAB, set `host` to the address of the pi and the `port` to the port of pigpio (default: 8888).

Note: If you are running Pigpio on same host as openHAB, then set host to **::1**.

### I2C - Bus  (`i2c-bus`)

Provides configuration and state for I2C bus instance with `id`. Raspberry provides two
busses with IDs `1` and `2`. Normally bus `1` is used.

### I2C - Device  (`i2c-device`)

Provides configuration for generic I2C device with `address`. Can be used meaningful for
discovery only, since no device protocol is implemented. Parameter `address` contains device
address on I2C-bus as hexadecimal.

### MCP23017 - Device  (`i2c-mcp23017`)

Provides configuration for MCP23017 device with `address`. Parameter `address` contains
device address on I2C-bus as hexadecimal. Parameter `refresh` represents polling interval for
input channels, if interrupt feature is not used. Additionally, it controls output channel
set time. The case interrupt feature is used, parameter `mirror` sets `INTA` and `INTB` pins
function with respect to each other. And parameter `mode` sets the polarity of `INT`-output pins:
  - HIGH: Output is pulled to high, if active, and low otherwise
  - LOW: Output is pulled to low, if active, and high otherwise
  - OPEN_DRAIN: Output is pulled to low, if active, and floating otherwise

## Channels

### Pigpio Remote

| channel        | type    | description                     |
|----------------|---------|---------------------------------|
| digital-input  | Contact | Read-only value of the gpio pin |
| digital-output | Switch  | Controls the gpio pin           |

For both channels number of the pin shall be configured in `gpioId`.
If you want to invert the value, set `activehigh` to false. For **digital-input** channels two
additional parameters are available: `debounce` and `pullupdown`. Preventing of incorrect changes
on input events is controlled with `debounce` parameter. Parameter `pullupdown` controls 
pull up / pull down resistor setting:
  - OFF: Resistor is deactivated, pin may be floating
  - DOWN: Resistor is pulled to ground, pin is low as default
  - UP: Resistor is pulled to supply, pin is high as default

### MCP23017

| channel         | type    | description                  |
|-----------------|---------|------------------------------|
| interrupt       | Contact | Controls interrupt signal    |
| reset           | Switch  | Controls device reset signal |
| mcp23017-input  | Contact | Controls MCP23017 inputs     |
| mcp23017-output | Switch  | Controls MCP23017 outputs    |

If **interrupt** channel is configured, than readings of all MCP23017 input pins are done on
`gpioId` pin level change. Additionally, polling of MCP23017 input pins is disabled. If **interrupt**
channel has no `gpioId` parameter, then **mcp23017-input** channels will be polled. Parameter
`activehigh` reflects the interrupt detection condition. To work properly, every **mcp23017-input**
channel shall have properly configured `interrupt` parameter (see below). Preventing of incorrect
changes on `interrupt` channel is controlled with `debounce` parameter. Normally shall not be configured,
since controlled by chip self. Parameter `pullupdown` controls  pull up / pull down resistor setting:
  - OFF: Resistor is deactivated, pin may be floating
  - DOWN: Resistor is pulled to ground, pin is low as default
  - UP: Resistor is pulled to supply, pin is high as default

More over, valid wiring between MCP23017-`INT` and `gpioId` pins is required.

If **reset** channel has valid `gpioId` pin, then MCP23017 will be resetted. Parameter `activehigh`
controls the signal activation level. Additionally, valid wiring between MCP23017-`RESET` and
`gpioId` pins is required.

Channel **mcp23017-input** is used for configuration MCP23017 pins as input. As far MCP23017 provides
two ports (PORTA and PORTB) and 8 pins on port, valid `pin` parameter are: **GPA0** ... **GPA7** and
**GPB0** ... **GPB7**. If you want to use opener as input, set parameter `activehigh` to false. Parameter
`interrupt` controls interrupt-on-change feature of MCP23017 input pins:
  - OFF: Level change will cause no interrupt
  - LOW: Deviation from low level will cause interrupt
  - HIGH: Deviation from high level will cause interrupt
  - PREVIOUS: Any level change will cause interrupt
  
To work properly, valid wiring of MCP23017-`INT` pin is required. Additionally, **interrupt** channel
(see above) shall be configured. Parameter `pullup` activates internal MCP23017 pull-up resistors,
if set.

Channel **mcp23017-output** is used for configuration MCP23017 pins as output. As far MCP23017 provides
two ports (PORTA and PORTB) and 8 pins on port, valid `pin` parameter are: **GPA0** ... **GPA7** and
**GPB0** ... **GPB7**. Parameter `defval` controls the output state after power up or device reset.

## Full Example

demo.things:

```java
Bridge gpio:remote:sample-pi-1 "Sample-Pi 1" [ host="192.168.2.36", port=8888 ] {
  Channels:
    Type digital-input : sample-input-1 [ gpioId=10 ]
    Type digital-input : sample-input-2 [ gpioId=14, activehigh=false ]
    Type digital-output : sample-output-1 [ gpioId=3 ]
}

Bridge gpio:remote:sample-pi-2 "Sample-Pi 2" [ host="192.168.2.37", port=8888 ] {
  Channels:
    Type digital-input : sample-input-3 [ gpioId=16, debounce=20 ]
    Type digital-input : sample-input-4 [ gpioId=17, activehigh=false, debounce=5, pullupdown="UP" ]
    Type digital-output : sample-output-2 [ gpioId=4, activehigh=false ]
}

Bridge gpio:remote:raspberry "Raspberry PI" [ host="127.0.0.1", port=8888 ] {
  Bridge i2c-bus i2c-1 "I2C Bus 1" [ id=1 ] {
    Thing i2c-mcp23017 mcp23017 "MCP23017 0x20" [address="0x20"] {
      Channels:
        Type digital-input : interrupt [ gpioId=22 ]
        Type digital-output : reset [ gpioId=27 ]
        Type mcp23017-input : GPA0 [ pin="GPA0" ]
        Type mcp23017-output : GPB0 [ pin="GPB0" ]
    }
  }
  Channels:
    Type digital-input : interrupt [ gpioId=22 ]
    Type digital-output : reset [ gpioId=27 ]
}
```

demo.items:

```java
Contact SampleInput1 {channel="gpio:remote:sample-pi-1:sample-input-1"}
Switch SampleOutput1 {channel="gpio:remote:sample-pi-1:sample-output-1"}
```

demo.sitemap:

```perl
sitemap demo label="Main Menu"
{
    Switch item=SampleInput1
    Switch item=SampleOutput1
}
```
