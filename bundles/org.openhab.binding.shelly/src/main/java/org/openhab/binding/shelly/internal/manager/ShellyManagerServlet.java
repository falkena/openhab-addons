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
package org.openhab.binding.shelly.internal.manager;

import static org.openhab.binding.shelly.internal.manager.ShellyManagerConstants.*;
import static org.openhab.binding.shelly.internal.util.ShellyUtils.*;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Serial;
import java.util.Map;

import javax.servlet.Servlet;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.binding.shelly.internal.ShellyHandlerFactory;
import org.openhab.binding.shelly.internal.api.ShellyApiException;
import org.openhab.binding.shelly.internal.manager.ShellyManagerPage.ShellyMgrResponse;
import org.openhab.binding.shelly.internal.provider.ShellyTranslationProvider;
import org.openhab.core.io.net.http.HttpClientFactory;
import org.openhab.core.net.HttpServiceUtil;
import org.openhab.core.net.NetworkAddressService;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.component.ComponentContext;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.ConfigurationPolicy;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.http.whiteboard.propertytypes.HttpWhiteboardServletName;
import org.osgi.service.http.whiteboard.propertytypes.HttpWhiteboardServletPattern;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * {@link ShellyManagerServlet} implements the Shelly Manager - a simple device overview/management
 *
 * @author Markus Michels - Initial contribution
 */
@NonNullByDefault
@Component(service = Servlet.class, configurationPolicy = ConfigurationPolicy.OPTIONAL)
@HttpWhiteboardServletName(ShellyManagerServlet.SERVLET_URI)
@HttpWhiteboardServletPattern(ShellyManagerServlet.SERVLET_URI + "/*")
public class ShellyManagerServlet extends HttpServlet {
    @Serial
    private static final long serialVersionUID = 1393403713585449126L;
    private final Logger logger = LoggerFactory.getLogger(ShellyManagerServlet.class);

    public static final String SERVLET_URI = SHELLY_MANAGER_URI;
    private final ShellyManager manager;
    private final String className;

    @Activate
    public ShellyManagerServlet(@Reference ConfigurationAdmin configurationAdmin,
            @Reference NetworkAddressService networkAddressService, @Reference HttpClientFactory httpClientFactory,
            @Reference ShellyHandlerFactory handlerFactory, @Reference ShellyTranslationProvider translationProvider,
            ComponentContext componentContext) {
        className = substringAfterLast(getClass().toString(), ".");
        String localIp = getString(networkAddressService.getPrimaryIpv4HostAddress());
        int localPort = HttpServiceUtil.getHttpServicePort(componentContext.getBundleContext());
        this.manager = new ShellyManager(configurationAdmin, translationProvider,
                httpClientFactory.getCommonHttpClient(), localIp, localPort, handlerFactory);

        // Promote Shelly Manager usage
        logger.info("{}", translationProvider.get("status.managerstarted", localIp, Integer.toString(localPort)));
    }

    @Deactivate
    protected void deactivate() {
        logger.debug("{} stopped", className);
    }

    @Override
    protected void service(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        String ipAddress = request.getHeader("HTTP_X_FORWARDED_FOR");
        if (ipAddress == null) {
            ipAddress = request.getRemoteAddr();
        }

        Map<String, String[]> parameters = request.getParameterMap();
        String path = getString(request.getRequestURI()).toLowerCase();
        logger.debug("{}: {} Request from {}:{}{}?{}", className, request.getProtocol(), ipAddress,
                request.getRemotePort(), path, parameters.toString());
        if (!path.toLowerCase().startsWith(SERVLET_URI)) {
            logger.warn("{} received unknown request: path = {}", className, path);
            return;
        }

        try {
            final ShellyMgrResponse output = manager.generateContent(path, parameters);
            response.setContentType(output.mimeType);
            if ("text/html".equals(output.mimeType)) {
                // Make sure it's UTF-8 encoded
                response.setCharacterEncoding(UTF_8);
                try (final PrintWriter writer = response.getWriter()) {
                    writer.write(output.toString());
                } catch (IOException ignored) {
                }
            } else {
                final byte[] data = output.data;
                if (data != null) {
                    response.setContentLength(data.length);
                    try (final OutputStream writer = response.getOutputStream()) {
                        writer.write(data, 0, data.length);
                    } catch (IOException ignored) {
                    }
                }
            }
        } catch (ShellyApiException | RuntimeException exception) {
            response.setContentType("text/html");
            try (final PrintWriter writer = response.getWriter()) {
                writer.write("Exception:" + exception + "<br/>Check openhab.log for details."
                        + "<p/><a href=\"/shelly/manager\">Return to Overview</a>");
            } catch (IOException ignored) {
            }
            logger.debug("{}: Exception uri={}, parameters={}", className, path, request.getParameterMap().toString());
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }
}
