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

import oshi.hardware.VirtualMemory;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoMockedVirtualMemory implements VirtualMemory {
    public static final long TEST_SWAP_AVAILABLE = 482;
    public static final long TEST_SWAP_TOTAL = 512;
    public static final long TEST_SWAP_USED = TEST_SWAP_TOTAL - TEST_SWAP_AVAILABLE;
    public static final long TEST_SWAP_PAGES_IN = 10;
    public static final long TEST_SWAP_PAGES_OUT = 20;
    public static final long TEST_VIRTUAL_AVAILABLE = 768;
    public static final long TEST_VIRTUAL_MAX = 1024;
    public static final long TEST_VIRTUAL_USE = TEST_VIRTUAL_MAX - TEST_VIRTUAL_AVAILABLE;

    @Override
    public long getSwapPagesIn() {
        return TEST_SWAP_PAGES_IN;
    }

    @Override
    public long getSwapPagesOut() {
        return TEST_SWAP_PAGES_OUT;
    }

    @Override
    public long getSwapTotal() {
        return TEST_SWAP_TOTAL;
    }

    @Override
    public long getSwapUsed() {
        return TEST_SWAP_USED;
    }

    @Override
    public long getVirtualMax() {
        return TEST_VIRTUAL_MAX;
    }

    @Override
    public long getVirtualInUse() {
        return TEST_VIRTUAL_USE;
    }
}
