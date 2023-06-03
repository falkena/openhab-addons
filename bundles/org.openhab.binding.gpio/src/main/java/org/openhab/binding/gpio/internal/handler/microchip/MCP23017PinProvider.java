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
package org.openhab.binding.gpio.internal.handler.microchip;

import static org.openhab.binding.gpio.internal.GPIOBindingConstants.MCP23017_CHANNEL_TYPE_INPUT;
import static org.openhab.binding.gpio.internal.GPIOBindingConstants.MCP23017_CHANNEL_TYPE_OUTPUT;
import static org.openhab.binding.gpio.internal.GPIOBindingConstants.MCP23017_GPIO_PINS;
import static org.openhab.binding.gpio.internal.GPIOBindingConstants.MCP23017_PIN_PREFIX;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.gpio.internal.configuration.microchip.MCP23017PinConfiguration;
import org.openhab.core.config.core.ConfigOptionProvider;
import org.openhab.core.config.core.ParameterOption;
import org.openhab.core.thing.binding.ThingHandler;
import org.openhab.core.thing.binding.ThingHandlerService;
import org.openhab.core.thing.type.ChannelTypeUID;

@NonNullByDefault
public class MCP23017PinProvider implements ConfigOptionProvider, ThingHandlerService {

    private @Nullable MCP23017Handler handler;

    @Override
    public void setThingHandler(@Nullable ThingHandler handler) {
        if (handler instanceof MCP23017Handler) {
            this.handler = (MCP23017Handler) handler;
        }
    }

    @Override
    public @Nullable MCP23017Handler getThingHandler() {
        return handler;
    }

    @Override
    public @Nullable Collection<ParameterOption> getParameterOptions(URI uri, String parameter,
            @Nullable String context, @Nullable Locale locale) {
        final MCP23017Handler handler = this.handler;
        if ("pin".equalsIgnoreCase(parameter) && (handler != null)) {
            final ChannelTypeUID type = new ChannelTypeUID(uri.getSchemeSpecificPart());
            if (MCP23017_CHANNEL_TYPE_INPUT.equals(type) || MCP23017_CHANNEL_TYPE_OUTPUT.equals(type)) {
                List<ParameterOption> options = new ArrayList<>();
                MCP23017_GPIO_PINS.forEach(value -> {
                    final String pin = value.replaceAll(MCP23017_PIN_PREFIX, "");
                    options.add(new ParameterOption(value, String.format("Input/Output %s", pin)));
                });
                handler.getThing().getChannels().forEach(channel -> {
                    final MCP23017PinConfiguration config = channel.getConfiguration()
                            .as(MCP23017PinConfiguration.class);
                    // Since i found no way how to inject ConfigDescriptionValidator into core,
                    // multiple pin configurations are enabled. Remove comment in next line to disable.
                    // options.removeIf(option -> option.getValue().equalsIgnoreCase(config.getPin()));
                });

                return options;
            } else {
                return null;
            }
        } else {
            return null;
        }
    }
}
