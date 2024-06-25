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
package org.openhab.binding.systeminfo.test.discovery;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.openhab.core.test.java.JavaOSGiTest;
import org.openhab.core.test.storage.VolatileStorageService;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class SystemInfoDeviceDiscoveryOSGiTest extends JavaOSGiTest {

    private @Nullable VolatileStorageService storageService;

    @BeforeEach
    public void setUp() {
        VolatileStorageService storageService = this.storageService;
        if (storageService != null) {
            unregisterService(storageService);
        }
        storageService = new VolatileStorageService();
        registerService(storageService);
        this.storageService = storageService;
    }

    @AfterEach
    public void tearDown() {
        final VolatileStorageService storageService = this.storageService;
        if (storageService != null) {
            unregisterService(storageService);
            this.storageService = null;
        }
    }
}
