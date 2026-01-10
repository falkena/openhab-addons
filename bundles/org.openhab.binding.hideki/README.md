# Hideki Binding

This binding provides native support for Hideki based weather stations, like Cresta, TFA-Dostmann and many others.
Two different wireless receivers are implemented now:

- Superheterodyne (RXB6, RXB8 and similar)
- Based on CC101 chip

## Supported Things

Receiver, Thermometer, Anemometer, Pluviometer and UV-meter

## Discovery

Discovery is not available.

## Binding Configuration

Shall be writte properly

## Thing Configuration

Shall be writte properly

## Channels

Shall be writte properly

## Examples

hideki.things:

```java
Bridge hideki:receiver:TFA [ receiver="CC1101", pin=21, device="/dev/spidev0.0", interrupt=0, refresh=1 ]
{
  Thing anemometer Anemometer "TFA Anemometer" @ "Weather station"
  Thing thermometer Thermometer "TFA Thermometer" @ "Weather station"
  Thing pluviometer Pluviometer "TFA Pluviometer" @ "Weather station"
  Thing uvmeter UVmeter "TFA UVmeter" @ "Weather station"
}
```

hideki.items:

```java
DateTime             HidekiThermometerUpdated     { channel="hideki:thermometer:TFA:Thermometer:updated" }
Number:Temperature   HidekiThermometerTemperature { channel="hideki:thermometer:TFA:Thermometer:temperature" }
Number:Dimensionless HidekiThermometerHumidity    { channel="hideki:thermometer:TFA:Thermometer:humidity" }
Switch               HidekiThermometerBattery     { channel="hideki:thermometer:TFA:Thermometer:battery" }
Number               HidekiThermometerId          { channel="hideki:thermometer:TFA:Thermometer:sensor" }
Number               HidekiThermometerChannel     { channel="hideki:thermometer:TFA:Thermometer:channel" }
Number               HidekiThermometerMessage     { channel="hideki:thermometer:TFA:Thermometer:message" }
Number               HidekiThermometerRSSI        { channel="hideki:thermometer:TFA:Thermometer:rssi" }

DateTime           HidekiAnemometerUpdated     { channel="hideki:anemometer:TFA:Anemometer:updated" }
Number:Temperature HidekiAnemometerTemperature { channel="hideki:anemometer:TFA:Anemometer:temperature" }
Number:Temperature HidekiAnemometerWindChill   { channel="hideki:anemometer:TFA:Anemometer:chill" }
Number:Speed       HidekiAnemometerWindGust    { channel="hideki:anemometer:TFA:Anemometer:gust" }
Number:Speed       HidekiAnemometerWindSpeed   { channel="hideki:anemometer:TFA:Anemometer:speed" }
Number:Angle       HidekiAnemometerDirection   { channel="hideki:anemometer:TFA:Anemometer:direction" }
Switch             HidekiAnemometerBattery     { channel="hideki:anemometer:TFA:Anemometer:battery" }
Number             HidekiAnemometerId          { channel="hideki:anemometer:TFA:Anemometer:sensor" }
Number             HidekiAnemometerMessage     { channel="hideki:anemometer:TFA:Anemometer:message" }
Number             HidekiAnemometerRSSI        { channel="hideki:anemometer:TFA:Anemometer:rssi" }

DateTime HidekiPluviometerUpdated   { channel="hideki:pluviometer:TFA:Pluviometer:updated" }
Number   HidekiPluviometerRainLevel { channel="hideki:pluviometer:TFA:Pluviometer:rain" }
Switch   HidekiPluviometerBattery   { channel="hideki:pluviometer:TFA:Pluviometer:battery" }
Number   HidekiPluviometerId        { channel="hideki:pluviometer:TFA:Pluviometer:sensor" }
Number   HidekiPluviometerMessage   { channel="hideki:pluviometer:TFA:Pluviometer:message" }
Number   HidekiPluviometerRSSI      { channel="hideki:pluviometer:TFA:Pluviometer:rssi" }

DateTime           HidekiUVmeterUpdated     { channel="hideki:uvmeter:TFA:UVmeter:updated" }
Number:Temperature HidekiUVmeterTemperature { channel="hideki:uvmeter:TFA:UVmeter:temperature" }
Number             HidekiUVmeterUVIndex     { channel="hideki:uvmeter:TFA:UVmeter:uv" }
Number             HidekiUVmeterMED         { channel="hideki:uvmeter:TFA:UVmeter:med" }
Switch             HidekiUVmeterBattery     { channel="hideki:uvmeter:TFA:UVmeter:battery" }
Number             HidekiUVmeterId          { channel="hideki:uvmeter:TFA:UVmeter:sensor" }
Number             HidekiUVmeterMessage     { channel="hideki:uvmeter:TFA:UVmeter:message" }
Number             HidekiUVmeterRSSI        { channel="hideki:uvmeter:TFA:UVmeter:rssi" }
```
