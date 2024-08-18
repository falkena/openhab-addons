/**
 * Copyright (c) 2010-2024 Contributors to the openHAB project
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
import java.util.concurrent.locks.StampedLock;

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
    private final StampedLock lock = new StampedLock();

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
        final long stamp = lock.readLock();
        try {
            return highPriorityChannels;
        } finally {
            lock.unlockRead(stamp);
        }
    }

    public Set<ChannelUID> getMediumPriorityChannels() {
        final long stamp = lock.readLock();
        try {
            return mediumPriorityChannels;
        } finally {
            lock.unlockRead(stamp);
        }
    }

    public Set<ChannelUID> getLowPriorityChannels() {
        final long stamp = lock.readLock();
        try {
            return lowPriorityChannels;
        } finally {
            lock.unlockRead(stamp);
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
        final long stamp = lock.writeLock();
        try {
            mediumPriorityChannels.remove(channelUID);
            lowPriorityChannels.remove(channelUID);
            highPriorityChannels.remove(channelUID);
            switch (priority) {
                case "High" -> highPriorityChannels.add(channelUID);
                case "Medium" -> mediumPriorityChannels.add(channelUID);
                case "Low" -> lowPriorityChannels.add(channelUID);
                default -> logger.debug("Invalid priority configuration parameter for channel {}", channelUID);
            }
        } finally {
            lock.unlockWrite(stamp);
        }
    }
}
