/*
 * Copyright (c) 2010-2025 Contributors to the openHAB project
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
package org.openhab.binding.mqtt.homeassistant.internal.component;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.mqtt.generic.ChannelStateUpdateListener;
import org.openhab.binding.mqtt.generic.values.OnOffValue;
import org.openhab.binding.mqtt.generic.values.PercentageValue;
import org.openhab.binding.mqtt.generic.values.TextValue;
import org.openhab.binding.mqtt.homeassistant.internal.ComponentChannel;
import org.openhab.binding.mqtt.homeassistant.internal.ComponentChannelType;
import org.openhab.binding.mqtt.homeassistant.internal.HomeAssistantChannelTransformation;
import org.openhab.binding.mqtt.homeassistant.internal.exception.UnsupportedComponentException;
import org.openhab.core.library.types.DecimalType;
import org.openhab.core.library.types.HSBType;
import org.openhab.core.library.types.OnOffType;
import org.openhab.core.library.types.PercentType;
import org.openhab.core.library.types.QuantityType;
import org.openhab.core.library.types.StringType;
import org.openhab.core.library.unit.Units;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.type.AutoUpdatePolicy;
import org.openhab.core.types.Command;
import org.openhab.core.types.State;
import org.openhab.core.types.UnDefType;
import org.openhab.core.util.ColorUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A MQTT light, following the https://www.home-assistant.io/components/light.mqtt/ specification.
 *
 * Specifically, the template schema. All channels are synthetic, and wrap the single internal raw
 * state.
 *
 * @author Cody Cutrer - Initial contribution
 */
@NonNullByDefault
public class TemplateSchemaLight extends AbstractRawSchemaLight {
    private final Logger logger = LoggerFactory.getLogger(TemplateSchemaLight.class);
    private @Nullable HomeAssistantChannelTransformation commandOnTransformation, commandOffTransformation,
            stateTransformation, brightnessTransformation, redTransformation, greenTransformation, blueTransformation,
            effectTransformation, colorTempTransformation;

    private static class TemplateVariables {
        public static final String STATE = "state";
        public static final String TRANSITION = "transition";
        public static final String BRIGHTNESS = "brightness";
        public static final String COLOR_TEMP = "color_temp";
        public static final String RED = "red";
        public static final String GREEN = "green";
        public static final String BLUE = "blue";
        public static final String HUE = "hue";
        public static final String SAT = "sat";
        public static final String FLASH = "flash";
        public static final String EFFECT = "effect";
    }

    public TemplateSchemaLight(ComponentFactory.ComponentConfiguration builder) {
        super(builder);
    }

    @Override
    protected void buildChannels() {
        AutoUpdatePolicy autoUpdatePolicy = optimistic ? AutoUpdatePolicy.RECOMMEND : null;
        String commandOnTemplate = channelConfiguration.commandOnTemplate,
                commandOffTemplate = channelConfiguration.commandOffTemplate;
        if (commandOnTemplate == null || commandOffTemplate == null) {
            throw new UnsupportedComponentException("Template schema light component '" + getHaID()
                    + "' does not define command_on_template or command_off_template!");
        }

        onOffValue = new OnOffValue("on", "off");
        brightnessValue = new PercentageValue(null, new BigDecimal(255), null, null, null, FORMAT_INTEGER);
        commandOnTransformation = new HomeAssistantChannelTransformation(getPython(), this, commandOnTemplate, true);
        commandOffTransformation = new HomeAssistantChannelTransformation(getPython(), this, commandOffTemplate, true);

        String redTemplate = channelConfiguration.redTemplate, greenTemplate = channelConfiguration.greenTemplate,
                blueTemplate = channelConfiguration.blueTemplate,
                brightnessTemplate = channelConfiguration.brightnessTemplate;
        if (redTemplate != null && greenTemplate != null && blueTemplate != null) {
            redTransformation = new HomeAssistantChannelTransformation(getPython(), this, redTemplate, false);
            greenTransformation = new HomeAssistantChannelTransformation(getPython(), this, greenTemplate, false);
            blueTransformation = new HomeAssistantChannelTransformation(getPython(), this, blueTemplate, false);
            colorChannel = buildChannel(COLOR_CHANNEL_ID, ComponentChannelType.COLOR, colorValue, "Color", this)
                    .commandTopic(DUMMY_TOPIC, true, 1).commandFilter(command -> handleCommand(command))
                    .withAutoUpdatePolicy(autoUpdatePolicy).build();
        } else if (brightnessTemplate != null) {
            brightnessTransformation = new HomeAssistantChannelTransformation(getPython(), this, brightnessTemplate,
                    false);
            brightnessChannel = buildChannel(BRIGHTNESS_CHANNEL_ID, ComponentChannelType.DIMMER, brightnessValue,
                    "Brightness", this).commandTopic(DUMMY_TOPIC, true, 1)
                    .commandFilter(command -> handleCommand(command)).withAutoUpdatePolicy(autoUpdatePolicy).build();
        } else {
            onOffChannel = buildChannel(SWITCH_CHANNEL_ID, ComponentChannelType.SWITCH, onOffValue, "On/Off State",
                    this).commandTopic(DUMMY_TOPIC, true, 1).commandFilter(command -> handleCommand(command))
                    .withAutoUpdatePolicy(autoUpdatePolicy).build();
        }

        String colorTempTemplate = channelConfiguration.colorTempTemplate;
        if (colorTempTemplate != null) {
            colorTempTransformation = new HomeAssistantChannelTransformation(getPython(), this, colorTempTemplate,
                    false);

            buildChannel(COLOR_TEMP_CHANNEL_ID, ComponentChannelType.NUMBER, colorTempValue, "Color Temperature", this)
                    .commandTopic(DUMMY_TOPIC, true, 1).commandFilter(command -> handleColorTempCommand(command))
                    .withAutoUpdatePolicy(autoUpdatePolicy).build();
        }
        TextValue localEffectValue = effectValue;
        String effectTemplate = channelConfiguration.effectTemplate;
        if (effectTemplate != null && localEffectValue != null) {
            effectTransformation = new HomeAssistantChannelTransformation(getPython(), this, effectTemplate, false);

            buildChannel(EFFECT_CHANNEL_ID, ComponentChannelType.STRING, localEffectValue, "Effect", this)
                    .commandTopic(DUMMY_TOPIC, true, 1).commandFilter(command -> handleEffectCommand(command))
                    .withAutoUpdatePolicy(autoUpdatePolicy).build();
        }

        String stateTemplate = channelConfiguration.stateTemplate;
        if (stateTemplate != null) {
            stateTransformation = new HomeAssistantChannelTransformation(getPython(), this, stateTemplate, false);
        }
    }

    private static BigDecimal factor = new BigDecimal("2.55"); // string to not lose precision

    @Override
    protected void publishState(HSBType state) {
        Map<String, @Nullable Object> binding = new HashMap<>();
        HomeAssistantChannelTransformation transformation;

        logger.trace("Publishing new state {} of light {} to MQTT.", state, getName());
        if (state.getBrightness().equals(PercentType.ZERO)) {
            transformation = Objects.requireNonNull(commandOffTransformation);
            binding.put(TemplateVariables.STATE, "off");
        } else {
            transformation = Objects.requireNonNull(commandOnTransformation);
            binding.put(TemplateVariables.STATE, "on");
            if (channelConfiguration.brightnessTemplate != null) {
                binding.put(TemplateVariables.BRIGHTNESS,
                        state.getBrightness().toBigDecimal().multiply(factor).intValue());
            }
            if (colorChannel != null) {
                int[] rgb = ColorUtil.hsbToRgb(state);
                binding.put(TemplateVariables.RED, rgb[0]);
                binding.put(TemplateVariables.GREEN, rgb[1]);
                binding.put(TemplateVariables.BLUE, rgb[2]);
                binding.put(TemplateVariables.HUE, state.getHue().toBigDecimal());
                binding.put(TemplateVariables.SAT, state.getSaturation().toBigDecimal());
            }
        }

        publishState(binding, transformation);
    }

    private boolean handleColorTempCommand(Command command) {
        if (command instanceof DecimalType) {
            command = new QuantityType<>(((DecimalType) command).toBigDecimal(), Units.MIRED);
        }
        if (command instanceof QuantityType quantity) {
            QuantityType<?> mireds = quantity.toInvertibleUnit(Units.MIRED);
            if (mireds == null) {
                logger.warn("Unable to convert {} to mireds", command);
                return false;
            }

            Map<String, @Nullable Object> binding = new HashMap<>();

            binding.put(TemplateVariables.STATE, "on");
            binding.put(TemplateVariables.COLOR_TEMP, mireds.toBigDecimal().intValue());

            publishState(binding, Objects.requireNonNull(commandOnTransformation));
        }
        return false;
    }

    private boolean handleEffectCommand(Command command) {
        if (!(command instanceof StringType)) {
            return false;
        }

        Map<String, @Nullable Object> binding = new HashMap<>();

        binding.put(TemplateVariables.STATE, "on");
        binding.put(TemplateVariables.EFFECT, command.toString());

        publishState(binding, Objects.requireNonNull(commandOnTransformation));
        return false;
    }

    private void publishState(Map<String, @Nullable Object> binding,
            HomeAssistantChannelTransformation transformation) {
        String command;

        command = transform(transformation, binding);
        if (command == null) {
            return;
        }

        logger.debug("Publishing new state '{}' of light {} to MQTT.", command, getHaID().toShortTopic());
        rawChannel.getState().publishValue(new StringType(command));
    }

    @Override
    public void updateChannelState(ChannelUID channel, State state) {
        ChannelStateUpdateListener listener = this.channelStateUpdateListener;

        String value;

        HomeAssistantChannelTransformation stateTransformation = this.stateTransformation;
        if (stateTransformation != null) {
            value = transform(stateTransformation, state.toString());
            if (value == null || value.isEmpty()) {
                onOffValue.update(UnDefType.NULL);
            } else if ("on".equals(value)) {
                onOffValue.update(OnOffType.ON);
            } else if ("off".equals(value)) {
                onOffValue.update(OnOffType.OFF);
            } else {
                logger.warn("Invalid state value '{}' for component {}; expected 'on' or 'off'.", value,
                        getHaID().toShortTopic());
                onOffValue.update(UnDefType.UNDEF);
            }
            if (brightnessValue.getChannelState() instanceof UnDefType
                    && !(onOffValue.getChannelState() instanceof UnDefType)) {
                brightnessValue.update(
                        (PercentType) Objects.requireNonNull(onOffValue.getChannelState().as(PercentType.class)));
            }
            if (colorValue.getChannelState() instanceof UnDefType
                    && onOffValue.getChannelState() instanceof OnOffType onOffValue) {
                colorValue.update(onOffValue);
            }
        }

        HomeAssistantChannelTransformation brightnessTransformation = this.brightnessTransformation;
        if (brightnessTransformation != null) {
            Integer brightness = getColorChannelValue(brightnessTransformation, state.toString());
            if (brightness == null) {
                brightnessValue.update(UnDefType.NULL);
                colorValue.update(UnDefType.NULL);
            } else {
                brightnessValue.update((PercentType) brightnessValue.parseMessage(new DecimalType(brightness)));
                if (colorValue.getChannelState() instanceof HSBType color) {
                    colorValue.update(new HSBType(color.getHue(), color.getSaturation(),
                            (PercentType) brightnessValue.getChannelState()));
                } else {
                    colorValue.update(new HSBType(DecimalType.ZERO, PercentType.ZERO,
                            (PercentType) brightnessValue.getChannelState()));
                }
            }
        }

        @Nullable
        HomeAssistantChannelTransformation redTransformation, greenTransformation, blueTransformation;
        if ((redTransformation = this.redTransformation) != null
                && (greenTransformation = this.greenTransformation) != null
                && (blueTransformation = this.blueTransformation) != null) {
            Integer red = getColorChannelValue(redTransformation, state.toString());
            Integer green = getColorChannelValue(greenTransformation, state.toString());
            Integer blue = getColorChannelValue(blueTransformation, state.toString());
            if (red == null || green == null || blue == null) {
                colorValue.update(UnDefType.NULL);
            } else {
                colorValue.update(HSBType.fromRGB(red, green, blue));
            }
        }
        ComponentChannel localBrightnessChannel = brightnessChannel;
        ComponentChannel localColorChannel = colorChannel;
        if (localColorChannel != null) {
            listener.updateChannelState(localColorChannel.getChannel().getUID(), colorValue.getChannelState());
        } else if (localBrightnessChannel != null) {
            listener.updateChannelState(localBrightnessChannel.getChannel().getUID(),
                    brightnessValue.getChannelState());
        } else {
            listener.updateChannelState(onOffChannel.getChannel().getUID(), onOffValue.getChannelState());
        }

        HomeAssistantChannelTransformation effectTransformation = this.effectTransformation;
        if (effectTransformation != null) {
            value = transform(effectTransformation, state.toString());
            if (value == null || value.isEmpty()) {
                effectValue.update(UnDefType.NULL);
            } else {
                effectValue.update(new StringType(value));
            }
            listener.updateChannelState(buildChannelUID(EFFECT_CHANNEL_ID), effectValue.getChannelState());
        }

        HomeAssistantChannelTransformation colorTempTransformation = this.colorTempTransformation;
        if (colorTempTransformation != null) {
            Integer mireds = getColorChannelValue(colorTempTransformation, state.toString());
            if (mireds == null) {
                colorTempValue.update(UnDefType.NULL);
            } else {
                colorTempValue.update(new QuantityType(mireds, Units.MIRED));
            }
            listener.updateChannelState(buildChannelUID(COLOR_TEMP_CHANNEL_ID), colorTempValue.getChannelState());
        }
    }

    private @Nullable Integer getColorChannelValue(HomeAssistantChannelTransformation transformation, String value) {
        Object result = transform(transformation, value);
        if (result == null) {
            return null;
        }

        String string = result.toString();
        if (string.isEmpty()) {
            return null;
        }
        try {
            return Integer.parseInt(result.toString());
        } catch (NumberFormatException e) {
            logger.warn("Applying template for component {} failed: {}", getHaID().toShortTopic(), e.getMessage());
            return null;
        }
    }

    private @Nullable String transform(HomeAssistantChannelTransformation transformation,
            Map<String, @Nullable Object> variables) {
        Object result = transformation.transform("", variables);
        if (result == null) {
            return null;
        }
        return result.toString();
    }

    private @Nullable String transform(HomeAssistantChannelTransformation transformation, String value) {
        return transformation.apply(value).orElse(null);
    }
}
