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
package org.openhab.binding.systeminfo.test;

import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.BINDING_ID;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.core.thing.type.ChannelTypeUID;

/**
 * SystemInfoOSGiTestConstants * used across the integration test.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoOSGiTestConstants {
    public static final ChannelTypeUID CHANNEL_TYPE_BYTES = new ChannelTypeUID(BINDING_ID, "bytes");
    public static final ChannelTypeUID CHANNEL_TYPE_COUNT = new ChannelTypeUID(BINDING_ID, "count");
    public static final ChannelTypeUID CHANNEL_TYPE_DESCRIPTION = new ChannelTypeUID(BINDING_ID, "description");
    public static final ChannelTypeUID CHANNEL_TYPE_NAME = new ChannelTypeUID(BINDING_ID, "name");

    public static final ChannelTypeUID CHANNEL_TYPE_FREQUENCY = new ChannelTypeUID(BINDING_ID, "frequency");
    public static final ChannelTypeUID CHANNEL_TYPE_MAX_FREQUENCY = new ChannelTypeUID(BINDING_ID, "maxFrequency");
    public static final ChannelTypeUID CHANNEL_TYPE_TEMPERATURE = new ChannelTypeUID(BINDING_ID, "temperature");
    public static final ChannelTypeUID CHANNEL_TYPE_VOLTAGE = new ChannelTypeUID(BINDING_ID, "voltage");

    public static final ChannelTypeUID CHANNEL_TYPE_MODEL = new ChannelTypeUID(BINDING_ID, "model");
    public static final ChannelTypeUID CHANNEL_TYPE_SERIAL = new ChannelTypeUID(BINDING_ID, "serial");

    public static final ChannelTypeUID CHANNEL_TYPE_TOTAL = new ChannelTypeUID(BINDING_ID, "total");
    public static final ChannelTypeUID CHANNEL_TYPE_PERCENT = new ChannelTypeUID(BINDING_ID, "percent");

    public static final ChannelTypeUID CHANNEL_TYPE_IP = new ChannelTypeUID(BINDING_ID, "ip");
    public static final ChannelTypeUID CHANNEL_TYPE_MAC = new ChannelTypeUID(BINDING_ID, "mac");

    public static final ChannelTypeUID CHANNEL_TYPE_TYPE = new ChannelTypeUID(BINDING_ID, "type");

    public static final ChannelTypeUID CHANNEL_TYPE_LOAD = new ChannelTypeUID(BINDING_ID, "load");
    public static final ChannelTypeUID CHANNEL_TYPE_LOAD_AVERAGE = new ChannelTypeUID(BINDING_ID, "loadAverage");
    public static final ChannelTypeUID CHANNEL_TYPE_THREADS = new ChannelTypeUID(BINDING_ID, "threads");
    public static final ChannelTypeUID CHANNEL_TYPE_UPTIME = new ChannelTypeUID(BINDING_ID, "uptime");
}
