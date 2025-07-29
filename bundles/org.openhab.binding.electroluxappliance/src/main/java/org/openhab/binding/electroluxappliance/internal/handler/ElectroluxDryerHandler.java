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
package org.openhab.binding.electroluxappliance.internal.handler;

import static org.openhab.binding.electroluxappliance.internal.ElectroluxApplianceBindingConstants.*;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.electroluxappliance.internal.dto.ApplianceDTO;
import org.openhab.binding.electroluxappliance.internal.dto.DryerStateDTO;
import org.openhab.core.i18n.LocaleProvider;
import org.openhab.core.i18n.TranslationProvider;
import org.openhab.core.library.types.OpenClosedType;
import org.openhab.core.thing.Channel;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusDetail;
import org.openhab.core.types.Command;
import org.openhab.core.types.RefreshType;
import org.openhab.core.types.State;
import org.openhab.core.types.UnDefType;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link ElectroluxDryerHandler} is responsible for handling commands and status updates for
 * Electrolux washing machines.
 *
 *
 * @author Jan Gustafsson - Initial contribution
 */
@NonNullByDefault
public class ElectroluxDryerHandler extends ElectroluxDoorApplianceHandler {

    private final Logger logger = LoggerFactory.getLogger(ElectroluxDryerHandler.class);

    public ElectroluxDryerHandler(Thing thing, @Reference TranslationProvider translationProvider,
            @Reference LocaleProvider localeProvider) {
        super(thing, translationProvider, localeProvider);
    }

    @Override
    public void handleCommand(ChannelUID channelUID, Command command) {
        logger.debug("Command received: {} on channelID: {}", command, channelUID);
        if (CHANNEL_STATUS.equals(channelUID.getId()) || command instanceof RefreshType) {
            super.handleCommand(channelUID, command);
        }
    }

    @Override
    public void update(@Nullable ApplianceDTO dto) {
        if (dto != null) {
            // Update all channels from the updated data
            getThing().getChannels().stream().map(Channel::getUID).filter(this::isLinked).forEach(channelUID -> {
                State state = getValue(channelUID.getId(), dto);
                logger.trace("Channel: {}, State: {}", channelUID, state);
                updateState(channelUID, state);
            });

            if (dto.isConnected()) {
                updateStatus(ThingStatus.ONLINE);
            } else {
                updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.COMMUNICATION_ERROR,
                        getLocalizedText("error.electroluxappliance.dryer.not-connected"));
            }
        }
    }

    private State getValue(String channelId, ApplianceDTO dto) {
        var reported = ((DryerStateDTO) dto.getApplianceState()).getProperties().getReported();
        return switch (channelId) {
            case CHANNEL_DOOR_STATE ->
                "OPEN".equals(reported.getDoorState()) ? OpenClosedType.OPEN : OpenClosedType.CLOSED;
            default -> UnDefType.UNDEF;
        };
    }

    @Override
    public Map<String, String> refreshProperties() {
        final var handler = getBridgeHandler();
        final var serial = getApplianceConfig().getSerialNumber();
        final var dto = (handler != null) ? handler.getElectroluxApplianceThings().get(serial) : null;

        Map<String, String> properties = new HashMap<>();
        if (dto != null) {
            var applianceInfo = dto.getApplianceInfo().getApplianceInfo();
            properties.put(Thing.PROPERTY_VENDOR, applianceInfo.getBrand());
            properties.put(PROPERTY_COLOUR, applianceInfo.getColour());
            properties.put(PROPERTY_DEVICE, applianceInfo.getDeviceType());
            properties.put(Thing.PROPERTY_MODEL_ID, applianceInfo.getModel());
            properties.put(Thing.PROPERTY_SERIAL_NUMBER, applianceInfo.getSerialNumber());
            properties.put(Thing.PROPERTY_FIRMWARE_VERSION, ((DryerStateDTO) dto.getApplianceState()).getProperties()
                    .getReported().getNetworkInterface().getSwVersion());
        }
        return properties;
    }
}
