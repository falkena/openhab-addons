/**
 * Copyright (c) 2010-2021 Contributors to the openHAB project
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
package org.openhab.binding.irobot.internal.discovery;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.BINDING_ID;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.BRIDGE_TYPE_CLOUD;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.THING_TYPE_BRAAVA_M;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.THING_TYPE_ROOMBA_9;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.THING_TYPE_ROOMBA_I;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.UDP_PORT;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.UNKNOWN;
import static org.openhab.binding.irobot.internal.IRobotHandlerFactory.SUPPORTED_THING_TYPES_UIDS;
import static org.openhab.binding.irobot.internal.utils.LoginRequester.getBlid;

import java.io.IOException;
import java.math.BigInteger;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.SecureRandom;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.openhab.binding.irobot.internal.dto.CloudRobotData;
import org.openhab.binding.irobot.internal.dto.Identification;
import org.openhab.binding.irobot.internal.handler.IRobotCloudHandler;
import org.openhab.core.config.discovery.AbstractDiscoveryService;
import org.openhab.core.config.discovery.DiscoveryResultBuilder;
import org.openhab.core.config.discovery.DiscoveryService;
import org.openhab.core.net.NetUtil;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.ThingHandler;
import org.openhab.core.thing.binding.ThingHandlerService;
import org.osgi.service.component.annotations.Component;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;

/**
 * Discovery service for iRobots. The {@link LoginRequester#getBlid} and
 * {@link IRobotDiscoveryService} are heavily related to each other.
 *
 * @author Pavel Fedin - Initial contribution
 * @author Alexander Falkenstern - Add support for I7 series
 *
 */
@NonNullByDefault
@Component(service = DiscoveryService.class, configurationPid = "discovery.irobot")
public class IRobotDiscoveryService extends AbstractDiscoveryService implements ThingHandlerService {

    private final Logger logger = LoggerFactory.getLogger(IRobotDiscoveryService.class);

    final Gson gson = new Gson();
    private @Nullable IRobotCloudHandler cloudHandler;

    private final Runnable scanner;
    private @Nullable ScheduledFuture<?> backgroundFuture;

    public IRobotDiscoveryService() {
        super(SUPPORTED_THING_TYPES_UIDS, 30, true);
        scanner = createScanner();
    }

    @Override
    public void setThingHandler(@Nullable ThingHandler handler) {
        removeOlderResults(System.currentTimeMillis());
        if (handler instanceof IRobotCloudHandler) {
            cloudHandler = (IRobotCloudHandler) handler;
        }
    }

    @Override
    public @Nullable ThingHandler getThingHandler() {
        return cloudHandler;
    }

    @Override
    public void activate() {
        removeOlderResults(System.currentTimeMillis());

        SecureRandom random = new SecureRandom();
        ThingUID bridgeUID = new ThingUID(BRIDGE_TYPE_CLOUD, Integer.toUnsignedString(random.nextInt(), 16));
        DiscoveryResultBuilder builder = DiscoveryResultBuilder.create(bridgeUID);
        builder = builder.withLabel("iRobot cloud");
        thingDiscovered(builder.build());

        super.activate(null);
    }

    @Override
    public void deactivate() {
        super.deactivate();
    }

    @Override
    protected void startBackgroundDiscovery() {
        stopBackgroundDiscovery();
        removeOlderResults(getTimestampOfLastScan());
        backgroundFuture = scheduler.scheduleWithFixedDelay(scanner, 0, 60, TimeUnit.SECONDS);
        super.startBackgroundDiscovery();
    }

    @Override
    protected void stopBackgroundDiscovery() {
        ScheduledFuture<?> scan = backgroundFuture;
        if (scan != null) {
            scan.cancel(true);
            backgroundFuture = null;
        }
    }

    @Override
    protected void startScan() {
        stopBackgroundDiscovery();
        scanner.run();
    }

    private Runnable createScanner() {
        return () -> {
            for (final Identification identification : getRobotsFromBroadcast()) {
                // Only firmware version 2 and above are supported via MQTT, therefore check it
                final String protocol = identification.getProto();
                final BigInteger version = identification.getVer();
                if ((BigInteger.ONE.compareTo(version) < 0) && "mqtt".equalsIgnoreCase(protocol)) {
                    final String address = identification.getIp();
                    final String mac = identification.getMac();
                    final String sku = identification.getSku();
                    String thingUID = BINDING_ID + ThingUID.SEPARATOR;
                    if (!address.isEmpty() && !sku.isEmpty() && !mac.isEmpty()) {
                        if (sku.regionMatches(true, 0, "M", 0, 1)) {
                            thingUID = thingUID + THING_TYPE_BRAAVA_M.getId() + ThingUID.SEPARATOR;
                        } else if (sku.regionMatches(true, 0, "E", 0, 1)) {
                            thingUID = thingUID + THING_TYPE_ROOMBA_I.getId() + ThingUID.SEPARATOR;
                        } else if (sku.regionMatches(true, 0, "I", 0, 1)) {
                            thingUID = thingUID + THING_TYPE_ROOMBA_I.getId() + ThingUID.SEPARATOR;
                        } else if (sku.regionMatches(true, 0, "R", 0, 1)) {
                            thingUID = thingUID + THING_TYPE_ROOMBA_9.getId() + ThingUID.SEPARATOR;
                        } else if (sku.regionMatches(true, 0, "S", 0, 1)) {
                            thingUID = thingUID + THING_TYPE_ROOMBA_I.getId() + ThingUID.SEPARATOR;
                        } else {
                            thingUID = null;
                        }
                    }

                    if (thingUID != null) {
                        CloudRobotData robot = null;
                        Bridge bridge = cloudHandler != null ? cloudHandler.getThing() : null;
                        if (bridge != null) {
                            thingUID = thingUID + bridge.getUID().getId() + ThingUID.SEPARATOR;
                            robot = cloudHandler.getRobots().get(getBlid(identification));
                        }
                        thingUID = thingUID + mac.replace(":", "");
                        DiscoveryResultBuilder builder = DiscoveryResultBuilder.create(new ThingUID(thingUID));
                        if (robot != null) {
                            builder = builder.withBridge(bridge.getUID());
                            builder = builder.withProperty("password", robot.getPassword());
                        }
                        builder = builder.withProperty("mac", mac).withRepresentationProperty("mac");
                        builder = builder.withProperty("address", address);

                        final String name = identification.getRobotname();
                        builder = builder.withLabel("iRobot " + (!name.isEmpty() ? name : UNKNOWN));
                        thingDiscovered(builder.build());
                    }
                }
            }
        };
    }

    private Set<Identification> getRobotsFromBroadcast() {
        Set<Identification> robots = new HashSet<>();
        for (String ip : NetUtil.getAllBroadcastAddresses()) {
            logger.debug("Starting broadcast for {}", ip);

            InetAddress address;
            try {
                address = InetAddress.getByName(ip);
            } catch (UnknownHostException exception) {
                logger.debug("Error sending broadcast: {}", exception.toString());
                continue;
            }

            try (DatagramSocket socket = new DatagramSocket()) {
                socket.setSoTimeout(5000); // Five seconds
                socket.setReuseAddress(true);
                socket.setBroadcast(true);

                final byte[] bRequest = "irobotmcs".getBytes(UTF_8);
                DatagramPacket request = new DatagramPacket(bRequest, bRequest.length, address, UDP_PORT);
                socket.send(request);

                final byte[] bReply = new byte[1024];
                DatagramPacket reply = new DatagramPacket(bReply, bReply.length);
                do {
                    try {
                        socket.receive(reply);
                        String json = new String(reply.getData(), reply.getOffset(), reply.getLength(), UTF_8);
                        final Identification identification = gson.fromJson(json, Identification.class);
                        if (identification != null) {
                            robots.add(identification);
                        }
                    } catch (IOException exception) {
                        // This is not really an error, eventually we get a timeout due to a loop in the caller
                        break;
                    }
                } while (reply.getLength() > 0);
            } catch (IOException exception) {
                logger.debug("Error sending broadcast: {}", exception.toString());
            }
        }

        return robots;
    }
}
