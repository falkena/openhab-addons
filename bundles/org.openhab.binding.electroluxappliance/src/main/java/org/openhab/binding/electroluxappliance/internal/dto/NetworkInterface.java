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
 * The {@link NetworkInterface} class defines the DTO for the Electrolux Washing Machines.
 *
 * @author Jan Gustafsson - Initial contribution
 */
@NonNullByDefault
public class NetworkInterface {
    private String swVersion = "";
    private String linkQualityIndicator = "";
    private String otaState = "";
    private String niuSwUpdateCurrentDescription = "";
    private String swAncAndRevision = "";

    public String getSwVersion() {
        return swVersion;
    }

    public String getLinkQualityIndicator() {
        return linkQualityIndicator;
    }

    public String getOtaState() {
        return otaState;
    }

    public String getNiuSwUpdateCurrentDescription() {
        return niuSwUpdateCurrentDescription;
    }

    public String getSwAncAndRevision() {
        return swAncAndRevision;
    }
}
