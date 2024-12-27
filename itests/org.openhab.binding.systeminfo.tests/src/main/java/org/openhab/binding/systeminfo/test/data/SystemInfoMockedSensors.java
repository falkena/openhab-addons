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
package org.openhab.binding.systeminfo.test.data;

import org.eclipse.jdt.annotation.NonNullByDefault;

import oshi.hardware.Sensors;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoMockedSensors implements Sensors {
    public static final double TEST_CPU_TEMPERATURE = 51.0;
    public static final double TEST_CPU_VOLTAGE = 230.0;

    @Override
    public double getCpuTemperature() {
        return TEST_CPU_TEMPERATURE;
    }

    @Override
    public int[] getFanSpeeds() {
        return new int[0];
    }

    @Override
    public double getCpuVoltage() {
        return TEST_CPU_VOLTAGE;
    }
}
