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
package org.openhab.binding.systeminfo.internal.handler;

import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_AVAILABLE;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_AVAILABLE_PERCENT;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_TOTAL;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_USED;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_USED_PERCENT;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.PRECISION_AFTER_DECIMAL_SIGN;

import java.math.BigDecimal;
import java.math.RoundingMode;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.core.library.types.QuantityType;
import org.openhab.core.library.unit.Units;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.types.State;
import org.openhab.core.types.UnDefType;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
class SystemInfoHandlerUtilities {
    public static State getChannelState(final ChannelUID channelUID, long available, long used, long total) {
        return (total <= 0) || (total < available) || (total < used) ? UnDefType.UNDEF
                : switch (channelUID.getIdWithoutGroup()) {
                    case CHANNEL_AVAILABLE -> new QuantityType<>(available, Units.BYTE);
                    case CHANNEL_AVAILABLE_PERCENT ->
                        new QuantityType<>(round((double) available / (double) total), Units.PERCENT);
                    case CHANNEL_TOTAL -> new QuantityType<>(total, Units.BYTE);
                    case CHANNEL_USED -> new QuantityType<>(used, Units.BYTE);
                    case CHANNEL_USED_PERCENT ->
                        new QuantityType<>(round((double) used / (double) total), Units.PERCENT);
                    default -> UnDefType.UNDEF;
                };
    }

    private static BigDecimal round(final double quotient) {
        final BigDecimal result = BigDecimal.valueOf(quotient * 100.0);
        return result.setScale(PRECISION_AFTER_DECIMAL_SIGN, RoundingMode.HALF_UP);
    }
}
