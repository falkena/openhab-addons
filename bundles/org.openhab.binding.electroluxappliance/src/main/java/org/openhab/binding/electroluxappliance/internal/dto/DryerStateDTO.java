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
package org.openhab.binding.electroluxappliance.internal.dto;

import org.eclipse.jdt.annotation.NonNullByDefault;

/**
 * The {@link DryerStateDTO} class defines the DTO for the Electrolux Washing Machines.
 *
 * @author Jan Gustafsson - Initial contribution
 */
@NonNullByDefault
public class DryerStateDTO extends ApplianceStateDTO {

    private final Properties properties = new Properties();

    public Properties getProperties() {
        return properties;
    }

    public static class Properties {
        private final Reported reported = new Reported();

        public Reported getReported() {
            return reported;
        }
    }

    public static class Reported {
        private String doorState = "";

        private final ApplianceInfo applianceInfo = new ApplianceInfo();
        private final String applianceMode = "";
        private final String applianceState = "";

        private final NetworkInterface networkInterface = new NetworkInterface();

        // Getters for all fields
        public String getDoorState() {
            return doorState;
        }

        public ApplianceInfo getApplianceInfo() {
            return applianceInfo;
        }

        public String getApplianceMode() {
            return applianceMode;
        }

        public String getApplianceState() {
            return applianceState;
        }

        public NetworkInterface getNetworkInterface() {
            return networkInterface;
        }
    }

    public static class ApplianceInfo {
        private String applianceType = "";

        public String getApplianceType() {
            return applianceType;
        }
    }
}
