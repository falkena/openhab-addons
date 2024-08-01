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

import java.math.BigDecimal;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.binding.BaseBridgeHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public abstract class SystemInfoBridgeScheduler extends BaseBridgeHandler {
    /**
     * Refresh interval for {@link #highPriorityChannels} in seconds.
     */
    private BigDecimal refreshIntervalHighPriority = BigDecimal.ZERO;

    /**
     * Refresh interval for {@link #mediumPriorityChannels} in seconds.
     */
    private BigDecimal refreshIntervalMediumPriority = BigDecimal.ZERO;

    /**
     * Channels with priority configuration parameter set to High. They usually need frequent update of the state like
     * CPU load, or information about the free and used memory.
     * They are updated periodically at {@link #refreshIntervalHighPriority}.
     */
    private final Set<ChannelUID> highPriorityChannels = new HashSet<>();

    /**
     * Channels with priority configuration parameter set to Medium. These channels usually need update of the
     * state not so oft like battery capacity, storage used and etc.
     * They are updated periodically at {@link #refreshIntervalMediumPriority}.
     */
    private final Set<ChannelUID> mediumPriorityChannels = new HashSet<>();

    /**
     * Channels with priority configuration parameter set to Low. They represent static information or information
     * that is updated rare- e.g. CPU name, storage name and etc.
     * They are updated only at {@link #initialize()}.
     */
    private final Set<ChannelUID> lowPriorityChannels = new HashSet<>();

    private final Logger logger = LoggerFactory.getLogger(SystemInfoBridgeScheduler.class);

    public SystemInfoBridgeScheduler(Bridge bridge) {
        super(bridge);
    }

    public int getHighPriorityRefreshInterval() {
        return refreshIntervalHighPriority.intValue();
    }

    public void setHighPriorityRefreshInterval(final BigDecimal value) {
        refreshIntervalHighPriority = value;
    }

    public int getMediumPriorityRefreshInterval() {
        return refreshIntervalMediumPriority.intValue();
    }

    public void setMediumPriorityRefreshInterval(final BigDecimal value) {
        refreshIntervalMediumPriority = value;
    }

    public Set<ChannelUID> getHighPriorityChannels() {
        synchronized (highPriorityChannels) {
            return Set.copyOf(highPriorityChannels);
        }
    }

    public Set<ChannelUID> getMediumPriorityChannels() {
        synchronized (mediumPriorityChannels) {
            return Set.copyOf(mediumPriorityChannels);
        }
    }

    public Set<ChannelUID> getLowPriorityChannels() {
        synchronized (lowPriorityChannels) {
            return Set.copyOf(lowPriorityChannels);
        }
    }

    public void changeChannelPriority(final ChannelUID channelUID, final Object priority) {
        if (priority instanceof String value) {
            changeChannelPriority(channelUID, value);
        } else {
            logger.debug("Invalid priority configuration parameter type for channel {}", channelUID);
        }
    }

    public void changeChannelPriority(final ChannelUID channelUID, final String priority) {
        synchronized (highPriorityChannels) {
            highPriorityChannels.remove(channelUID);
            if ("High".equalsIgnoreCase(priority)) {
                highPriorityChannels.add(channelUID);
            }
        }
        synchronized (mediumPriorityChannels) {
            mediumPriorityChannels.remove(channelUID);
            if ("Medium".equalsIgnoreCase(priority)) {
                mediumPriorityChannels.add(channelUID);
            }
        }
        synchronized (lowPriorityChannels) {
            lowPriorityChannels.remove(channelUID);
            if ("Low".equalsIgnoreCase(priority)) {
                lowPriorityChannels.add(channelUID);
            }
        }
    }
}
