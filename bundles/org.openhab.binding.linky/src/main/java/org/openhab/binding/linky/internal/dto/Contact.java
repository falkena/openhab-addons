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
package org.openhab.binding.linky.internal.dto;

/**
 * The {@link Contact} holds informations about the contact of a contract
 *
 * @author Laurent Arnal - Initial contribution
 */

public class Contact {
    public String phone;
    public String email;

    public static Contact convertFromUserInfo(UserInfo userInfo) {
        Contact result = new Contact();

        result.email = userInfo.userProperties.mail;

        return result;
    }
}
