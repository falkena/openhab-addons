/*
 * Copyright (c) 2010-2026 Contributors to the openHAB project
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
package org.openhab.binding.shelly.internal.manager;

import static org.openhab.binding.shelly.internal.manager.ShellyManagerConstants.IMAGE_PATH;
import static org.openhab.binding.shelly.internal.util.ShellyUtils.substringAfter;

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jetty.client.HttpClient;
import org.eclipse.jetty.http.HttpStatus;
import org.openhab.binding.shelly.internal.ShellyHandlerFactory;
import org.openhab.binding.shelly.internal.handler.ShellyManagerInterface;
import org.openhab.binding.shelly.internal.provider.ShellyTranslationProvider;
import org.osgi.service.cm.ConfigurationAdmin;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * {@link ShellyManagerImageLoader} implements the Shelly Manager's download proxy for images (load them from bundle)
 *
 * @author Markus Michels - Initial contribution
 */
@NonNullByDefault
public class ShellyManagerImageLoader extends ShellyManagerPage {
    private final Logger logger = LoggerFactory.getLogger(ShellyManagerImageLoader.class);

    public ShellyManagerImageLoader(ConfigurationAdmin configurationAdmin,
            ShellyTranslationProvider translationProvider, HttpClient httpClient, String localIp, int localPort,
            ShellyHandlerFactory handlerFactory, ShellyManagerCache<String, FwRepoEntry> firmwareRepo,
            ShellyManagerCache<String, FwArchList> firmwareArch) {
        super(configurationAdmin, translationProvider, httpClient, localIp, localPort, handlerFactory, firmwareRepo,
                firmwareArch);
    }

    @Override
    public ShellyMgrResponse generateContent(String path, Map<String, String[]> parameters) {
        return loadImage(substringAfter(path, ShellyManagerConstants.SHELLY_MGR_IMAGES_URI + "/"));
    }

    protected ShellyMgrResponse loadImage(String image) {
        String file = IMAGE_PATH + image;
        logger.trace("Read Image from {}", file);

        final String errorMessage = String.format("Unable to read %s from bundle resources", image);
        final ClassLoader loader = ShellyManagerInterface.class.getClassLoader();
        if (loader != null) {
            try (InputStream inputStream = loader.getResourceAsStream(file)) {
                if (inputStream != null) {
                    final byte[] buffer = new byte[inputStream.available()];
                    if (inputStream.read(buffer) == buffer.length) {
                        return new ShellyMgrResponse(buffer, HttpStatus.OK_200, "image/png");
                    } else {
                        return new ShellyMgrResponse(errorMessage, HttpStatus.PRECONDITION_FAILED_412);
                    }
                }
            } catch (IOException | RuntimeException ignored) {
                logger.debug("ShellyManager: {}", errorMessage);
            }
        }
        return new ShellyMgrResponse(errorMessage, HttpStatus.NOT_FOUND_404);
    }
}
