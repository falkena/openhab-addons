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

import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CACHE_REFRESH_INTERVAL_MS;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_ALLOCATED;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_LOAD;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_NAME;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_PROCESS_PATH;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_THREADS;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.CHANNEL_USED;
import static org.openhab.binding.systeminfo.internal.config.SystemInfoProcessConfig.CURRENT_PROCESS;
import static org.openhab.binding.systeminfo.internal.handler.SystemInfoHandlerUtilities.getPercent;
import static oshi.software.os.OperatingSystem.ProcessFiltering.ALL_PROCESSES;
import static oshi.software.os.OperatingSystem.ProcessSorting.PID_ASC;

import java.math.BigDecimal;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.systeminfo.internal.config.SystemInfoProcessConfig;
import org.openhab.binding.systeminfo.internal.model.SystemInfoInterface;
import org.openhab.core.cache.ExpiringCache;
import org.openhab.core.library.types.DecimalType;
import org.openhab.core.library.types.QuantityType;
import org.openhab.core.library.types.StringType;
import org.openhab.core.library.unit.Units;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusDetail;
import org.openhab.core.types.Command;
import org.openhab.core.types.RefreshType;
import org.openhab.core.types.State;
import org.openhab.core.types.UnDefType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import oshi.software.os.OSProcess;
import oshi.software.os.OperatingSystem;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoProcessHandler extends SystemInfoBaseThingHandler {
    private final Logger logger = LoggerFactory.getLogger(SystemInfoProcessHandler.class);

    private @Nullable OSProcess process;

    /**
     * Cache for cpu process load. Using this cache limits the process load refresh interval to the minimum interval.
     * Too frequent refreshes leads to inaccurate results. This could happen when the same process is tracked as
     * current process and as a channel with pid parameter, or when the task interval is set too low.
     */
    private final ExpiringCache<@Nullable BigDecimal> cpuLoad;

    public SystemInfoProcessHandler(Thing thing, SystemInfoInterface systemInfo) {
        super(thing, systemInfo);
        cpuLoad = new ExpiringCache<>(CACHE_REFRESH_INTERVAL_MS, this::getProcessCPULoad);
    }

    @Override
    public void initialize() {
        final Thing thing = getThing();
        logger.trace("Initializing thing {} with thing type {}", thing.getUID(), thing.getThingTypeUID());

        final OperatingSystem system = systemInfo.getOperatingSystem();
        final SystemInfoProcessConfig config = getConfigAs(SystemInfoProcessConfig.class);
        system.getProcesses(ALL_PROCESSES, PID_ASC, 0).stream().filter(entry -> {
            final String name = config.getProcessName();
            return name.equalsIgnoreCase(entry.getName());
        }).findFirst().ifPresentOrElse((process) -> {
            config.setProcessID(process.getProcessID());
            updateConfiguration(config.asConfiguration());

            this.process = process;
            updateStatus(ThingStatus.ONLINE);
            super.initialize();
        }, () -> {
            if (CURRENT_PROCESS.equalsIgnoreCase(config.getProcessName())) {
                final OSProcess process = system.getCurrentProcess();
                config.setProcessID(process.getProcessID());
                updateConfiguration(config.asConfiguration());

                this.process = process;
                updateStatus(ThingStatus.ONLINE);
                super.initialize();
            } else {
                this.process = null;
                updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.HANDLER_INITIALIZING_ERROR,
                        "@text/missing-or-invalid-configuration");
            }
        });
    }

    @Override
    public void dispose() {
        process = null;
        super.dispose();
    }

    @Override
    public void handleCommand(ChannelUID channelUID, Command command) {
        if (ThingStatus.ONLINE.equals(thing.getStatus())) {
            if (command instanceof RefreshType) {
                final OSProcess process = this.process;
                if ((process != null) && process.updateAttributes()) {
                    if (process.getProcessID() >= 0) {
                        final State state = switch (channelUID.getIdWithoutGroup()) {
                            case CHANNEL_ALLOCATED -> new QuantityType<>(process.getVirtualSize(), Units.BYTE);
                            case CHANNEL_LOAD -> {
                                final BigDecimal value = cpuLoad.getValue();
                                yield value != null ? new QuantityType<>(value, Units.PERCENT) : UnDefType.UNDEF;
                            }
                            case CHANNEL_NAME -> {
                                final int current = systemInfo.getOperatingSystem().getProcessId();
                                yield new StringType(
                                        process.getProcessID() == current ? CURRENT_PROCESS : process.getName());
                            }
                            case CHANNEL_THREADS -> new DecimalType(process.getThreadCount());
                            case CHANNEL_USED -> new QuantityType<>(process.getResidentSetSize(), Units.BYTE);
                            case CHANNEL_PROCESS_PATH -> new StringType(process.getPath());
                            default -> UnDefType.UNDEF;
                        };
                        updateState(channelUID, state);
                    } else {
                        updateState(channelUID, UnDefType.UNDEF);
                    }
                } else {
                    final String name = getConfigAs(SystemInfoProcessConfig.class).getProcessName();
                    logger.warn("Unable to refresh channel {} with process name {}", channelUID, name);
                    updateState(channelUID, UnDefType.UNDEF);
                }
            }
        }
    }

    private @Nullable BigDecimal getProcessCPULoad() {
        BigDecimal result = null;

        final SystemInfoProcessConfig config = getConfigAs(SystemInfoProcessConfig.class);
        final OSProcess process = systemInfo.getOperatingSystem().getProcess(config.getProcessID());
        if ((process != null) && (process.getProcessID() >= 0)) {
            result = getPercent(process.getProcessCpuLoadBetweenTicks(this.process));
        }
        this.process = process;

        return result;
    }
}
