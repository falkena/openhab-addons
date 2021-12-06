/**
 * Copyright (c) 2010-2021 Contributors to the openHAB project
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
package org.openhab.binding.irobot.internal;

import static org.openhab.binding.irobot.internal.IRobotBindingConstants.*;

import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.BaseDynamicStateDescriptionProvider;
import org.openhab.core.thing.type.DynamicStateDescriptionProvider;
import org.openhab.core.types.StateOption;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;

/**
 * The {@link IRobotChannelContentProvider} provides dynamic channel state descriptions.
 * Overrides the state description for the controls, which receive its configuration in the runtime.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
@Component(service = { DynamicStateDescriptionProvider.class, IRobotChannelContentProvider.class }, immediate = true)
public class IRobotChannelContentProvider extends BaseDynamicStateDescriptionProvider {

    @Activate
    public IRobotChannelContentProvider() {
    }

    public void setLanguages(final ChannelUID channelUID, final Map<String, String> languages) {
        final ThingUID thingUID = channelUID.getThingUID();
        if (BINDING_ID.equals(thingUID.getBindingId()) && !languages.isEmpty()) {
            if (CHANNEL_CONTROL_LANGUAGE.equals(channelUID.getIdWithoutGroup())) {
                List<StateOption> buffer = new ArrayList<>();
                for (final Map.Entry<String, String> language : languages.entrySet()) {
                    final Locale locale = Locale.forLanguageTag(language.getValue());
                    buffer.add(new StateOption(language.getKey(), locale.getDisplayName()));
                }
                buffer.add(new StateOption(UNKNOWN, "Unknown"));
                setStateOptions(channelUID, buffer);
            }
        }
    }

    public void setTimeZones(final ChannelUID channelUID) {
        final ThingUID thingUID = channelUID.getThingUID();
        if (BINDING_ID.equals(thingUID.getBindingId())) {
            if (CHANNEL_COMMON_TIMEZONE.equals(channelUID.getIdWithoutGroup())) {
                List<StateOption> buffer = new ArrayList<>();
                for (final String zoneId : ZoneId.getAvailableZoneIds()) {
                    buffer.add(new StateOption(zoneId.trim(), zoneId.trim()));
                }
                Collections.sort(buffer, Comparator.comparing(StateOption::getValue));
                buffer.add(new StateOption(UNKNOWN, "Unknown"));
                setStateOptions(channelUID, buffer);
            }
        }
    }

    public boolean isChannelPopulated(ChannelUID channelUID) {
        return channelOptionsMap.containsKey(channelUID);
    }
}
