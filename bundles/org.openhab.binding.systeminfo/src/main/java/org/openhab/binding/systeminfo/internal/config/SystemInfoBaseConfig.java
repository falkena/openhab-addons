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
package org.openhab.binding.systeminfo.internal.config;

/**
 * @author Alexander Falkenstern - Initial contribution
 */

import java.lang.reflect.Field;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.core.config.core.Configuration;

/**
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoBaseConfig {
    public Configuration asConfiguration() {
        final Configuration configuration = new Configuration();

        for (final Field field : this.getClass().getDeclaredFields()) {
            field.setAccessible(true);
            try {
                configuration.put(field.getName(), field.get(this));
            } catch (IllegalAccessException ignored) {

            }
        }

        return configuration;
    }
}
