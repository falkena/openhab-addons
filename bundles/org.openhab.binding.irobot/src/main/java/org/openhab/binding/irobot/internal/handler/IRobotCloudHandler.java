/**
 * Copyright (c) 2021 Alexander Falkenstern
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
package org.openhab.binding.irobot.internal.handler;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.UNKNOWN;

import java.io.IOException;
import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.Collection;
import java.util.Collections;
import java.util.Locale;
import java.util.Map;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.irobot.internal.config.IRobotCloudConfiguration;
import org.openhab.binding.irobot.internal.discovery.IRobotDiscoveryService;
import org.openhab.binding.irobot.internal.dto.CloudAccount;
import org.openhab.binding.irobot.internal.dto.CloudDeployment;
import org.openhab.binding.irobot.internal.dto.CloudEndpoints;
import org.openhab.binding.irobot.internal.dto.CloudRobotData;
import org.openhab.binding.irobot.internal.dto.CloudRobotsData;
import org.openhab.binding.irobot.internal.dto.Gigya;
import org.openhab.core.config.core.Configuration;
import org.openhab.core.i18n.LocaleProvider;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusDetail;
import org.openhab.core.thing.binding.BaseBridgeHandler;
import org.openhab.core.thing.binding.ThingHandlerService;
import org.openhab.core.types.Command;
import org.openhab.core.types.RefreshType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

/**
 * The {@link IRobotCloudHandler} is the handler for IRobot cloud and connects it to the framework.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class IRobotCloudHandler extends BaseBridgeHandler {
    private final Logger logger = LoggerFactory.getLogger(IRobotCloudHandler.class);

    private final Gson gson = new Gson();
    final HttpClient client = HttpClient.newHttpClient();
    private volatile @NonNullByDefault({}) IRobotCloudConfiguration config;

    private final LocaleProvider localeProvider;

    private volatile @Nullable CloudAccount account = null;
    private volatile @Nullable CloudEndpoints endpoints = null;
    private volatile @Nullable CloudRobotsData robots = null;

    public IRobotCloudHandler(Bridge bridge, LocaleProvider localeProvider) {
        super(bridge);
        this.localeProvider = localeProvider;
        config = getConfigAs(IRobotCloudConfiguration.class);
    }

    public @Nullable Map<String, CloudRobotData> getRobots() {
        return robots != null ? robots.getRobots() : null;
    }

    @Override
    public void handleCommand(ChannelUID channelUID, Command command) {
        logger.debug("Handle command {} on channel {}", command, channelUID);

        Bridge bridge = getThing();
        if (ThingStatus.ONLINE != bridge.getStatus()) {
            return;
        }

        if (!(command instanceof RefreshType)) {
            logger.debug("Not supported command {} received.", command);
            return;
        }
    }

    @Override
    public void initialize() {
        config = getConfigAs(IRobotCloudConfiguration.class);

        boolean configured = !UNKNOWN.equals(config.getEMail());
        configured = configured && !UNKNOWN.equals(config.getPassword());

        if (configured) {
            if (endpoints == null) {
                fetchEndpointData();
            }
            if (account == null) {
                fetchAccountData();
            }
            if (robots == null) {
                fetchRobotData();
            }
        } else {
            String message = "Can not initialize IRobot cloud connection";
            updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.CONFIGURATION_ERROR, message);
        }
    }

    @Override
    public void dispose() {
        robots = null;
        account = null;
        endpoints = null;
    }

    @Override
    public Collection<Class<? extends ThingHandlerService>> getServices() {
        return Collections.singleton(IRobotDiscoveryService.class);
    }

    @Override
    protected void updateConfiguration(Configuration configuration) {
        super.updateConfiguration(configuration);
        config = getConfigAs(IRobotCloudConfiguration.class);
        fetchAccountData();
    }

    private void fetchEndpointData() {
        try {
            final String country = localeProvider.getLocale().getCountry();
            final String code = country.isBlank() ? Locale.US.getCountry() : country;
            URI uri = URI.create("https://disc-prod.iot.irobotapi.com/v1/discover/endpoints?country_code=" + code);
            HttpRequest.Builder builder = HttpRequest.newBuilder(uri);
            builder = builder.header("Accept", "application/json");
            builder = builder.timeout(Duration.ofSeconds(config.getTimeout()));
            HttpResponse<String> response = client.send(builder.build(), HttpResponse.BodyHandlers.ofString());
            endpoints = gson.fromJson(response.body(), CloudEndpoints.class);
        } catch (IOException | InterruptedException exception) {
            endpoints = null;
            updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.COMMUNICATION_ERROR, exception.getMessage());
            scheduler.execute(this::fetchEndpointData);
        } catch (JsonSyntaxException exception) {
            endpoints = null;
            updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.CONFIGURATION_ERROR, exception.getMessage());
        }

        if (endpoints != null) {
            fetchAccountData();
        }
    }

    private void fetchAccountData() {
        try {
            final Gigya gigya = endpoints.getGigya();
            String request = String.format("apiKey=%s&targetEnv=mobile&format=json&", gigya.getApiKey());
            request += String.format("loginID=%s&", URLEncoder.encode(config.getEMail(), UTF_8));
            request += String.format("password=%s", URLEncoder.encode(config.getPassword(), UTF_8));

            URI uri = URI.create(String.format("https://accounts.%s/accounts.login", gigya.getDatacenterDomain()));
            HttpRequest.Builder builder = HttpRequest.newBuilder(uri);
            builder = builder.POST(HttpRequest.BodyPublishers.ofString(request));
            builder = builder.header("Content-Type", "application/x-www-form-urlencoded");
            builder = builder.timeout(Duration.ofSeconds(config.getTimeout()));
            HttpResponse<String> response = client.send(builder.build(), HttpResponse.BodyHandlers.ofString());
            account = gson.fromJson(response.body(), CloudAccount.class);
        } catch (IOException | InterruptedException exception) {
            account = null;
            updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.COMMUNICATION_ERROR, exception.getMessage());
            scheduler.execute(this::fetchAccountData);
        } catch (JsonSyntaxException exception) {
            account = null;
            updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.CONFIGURATION_ERROR, exception.getMessage());
        }

        if (account != null) {
            fetchRobotData();
        }
    }

    private void fetchRobotData() {
        try {
            String gigya = String.format("\"signature\": \"%s\", ", account.getUIDSignature());
            gigya += String.format("\"timestamp\": %s, ", account.getSignatureTimestamp());
            gigya += String.format("\"uid\": \"%s\"", account.getUid());
            String request = "{ \"app_id\": \"OpenHAB-iRobot-Binding\", \"assume_robot_ownership\": 0, ";
            request += String.format("\"gigya\": { %s } }", gigya);

            CloudDeployment deployment = endpoints.getDeployments().get(endpoints.getCurrentDeployment());
            URI uri = URI.create(String.format("%s/v2/login", deployment.getHttpBase()));
            HttpRequest.Builder builder = HttpRequest.newBuilder(uri);
            builder = builder.POST(HttpRequest.BodyPublishers.ofString(request));
            builder = builder.header("Content-Type", "application/json");
            builder = builder.timeout(Duration.ofSeconds(config.getTimeout()));
            HttpResponse<String> response = client.send(builder.build(), HttpResponse.BodyHandlers.ofString());
            robots = gson.fromJson(response.body(), CloudRobotsData.class);
        } catch (IOException | InterruptedException exception) {
            robots = null;
            updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.COMMUNICATION_ERROR, exception.getMessage());
            scheduler.execute(this::fetchRobotData);
        } catch (JsonSyntaxException exception) {
            robots = null;
            updateStatus(ThingStatus.OFFLINE, ThingStatusDetail.CONFIGURATION_ERROR, exception.getMessage());
        }

        if (robots != null) {
            updateStatus(ThingStatus.ONLINE);
        }
    }
}
