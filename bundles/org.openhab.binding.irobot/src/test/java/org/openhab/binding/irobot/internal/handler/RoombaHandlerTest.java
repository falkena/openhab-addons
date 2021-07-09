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
package org.openhab.binding.irobot.internal.handler;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CHANNEL_CONTROL_CLEAN_PASSES;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.CONTROL_GROUP_ID;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.PASSES_1;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.PASSES_2;
import static org.openhab.binding.irobot.internal.IRobotBindingConstants.PASSES_AUTO;

import java.io.IOException;
import java.lang.reflect.Field;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.openhab.binding.irobot.internal.config.IRobotConfiguration;
import org.openhab.binding.irobot.internal.dto.CleanPasses;
import org.openhab.core.config.core.Configuration;
import org.openhab.core.library.types.StringType;
import org.openhab.core.thing.ChannelGroupUID;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusDetail;
import org.openhab.core.thing.ThingStatusInfo;
import org.openhab.core.thing.ThingTypeUID;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.ThingHandlerCallback;
import org.openhab.core.thing.internal.ThingImpl;
import org.openhab.core.types.RefreshType;
import org.openhab.core.types.State;
import org.openhab.core.types.UnDefType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.spi.LocationAwareLogger;

/**
 * Test the MQTT protocol with local iRobot (without openhab running).
 * This class is used to test the binding against a local iRobot instance.
 *
 * @author Florian Binder - Initial contribution
 */

@NonNullByDefault
@ExtendWith(MockitoExtension.class)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
public class RoombaHandlerTest {

    class IRobotCommonHandlerMock extends IRobotCommonHandler {
        public IRobotCommonHandlerMock(Thing thing) {
            super(thing);
        }

        @Override
        public void updateState(ChannelUID channelUID, State state) {
            super.updateState(channelUID, state);
        }
    }

    private static final String IP_ADDRESS = "<iRobotIP>";
    private static final String PASSWORD = "<PasswordForIRobot>";

    private @Mock Thing thing = new ThingImpl(new ThingTypeUID("AA:BB"), "");
    private @NonNullByDefault({}) IRobotCommonHandlerMock handler;

    @BeforeEach
    void setUp() throws Exception {
        Logger logger = LoggerFactory.getLogger(IRobotCommonHandler.class);
        Field logLevelField = logger.getClass().getDeclaredField("currentLogLevel");
        logLevelField.setAccessible(true);
        logLevelField.set(logger, LocationAwareLogger.TRACE_INT);

        Configuration config = new Configuration();
        config.put("address", RoombaHandlerTest.IP_ADDRESS);
        config.put("password", RoombaHandlerTest.PASSWORD);
        Mockito.when(thing.getConfiguration()).thenReturn(config);
        Mockito.lenient().when(thing.getStatusInfo())
                .thenReturn(new ThingStatusInfo(ThingStatus.UNINITIALIZED, ThingStatusDetail.NONE, "mocked"));
        Mockito.lenient().when(thing.getUID()).thenReturn(new ThingUID("mocked", "irobot", "uid"));

        handler = Mockito.spy(new IRobotCommonHandlerMock(thing));
        handler.setCallback(Mockito.mock(ThingHandlerCallback.class));
    }

    @Test
    void testConfiguration() {
        handler.initialize();

        IRobotConfiguration config = thing.getConfiguration().as(IRobotConfiguration.class);
        assertEquals(config.getAddress(), IP_ADDRESS);
        assertEquals(config.getPassword(), PASSWORD);

        handler.dispose();
    }

    @Test
    void testCleanRegion() throws InterruptedException, IOException {
        handler.initialize();

        ChannelUID cmd = new ChannelUID(thing.getUID(), "command");
        handler.handleCommand(cmd, new StringType("cleanRegions:AABBCCDDEEFFGGHH;2,3"));

        handler.dispose();
    }

    @Test
    void testDock() throws InterruptedException, IOException {
        handler.initialize();

        ChannelUID cmd = new ChannelUID(thing.getUID(), "command");
        handler.handleCommand(cmd, new StringType("dock"));

        handler.dispose();
    }

    @Test
    void testStop() throws InterruptedException, IOException {
        handler.initialize();

        ChannelUID cmd = new ChannelUID(thing.getUID(), "command");
        handler.handleCommand(cmd, new StringType("stop"));

        handler.dispose();
    }

    @Test
    void testCleanPassesReceivedNoValues() {
        handler.initialize();

        CleanPasses passes = new CleanPasses(null, null);
        ChannelGroupUID controlGroupUID = new ChannelGroupUID(thing.getUID(), CONTROL_GROUP_ID);
        ChannelUID channelUID = new ChannelUID(controlGroupUID, CHANNEL_CONTROL_CLEAN_PASSES);

        doAnswer(invocation -> {
            assertEquals(UnDefType.UNDEF, invocation.getArgument(1));
            return null;
        }).when(handler).updateState(any(ChannelUID.class), any(State.class));
        Mockito.when(handler.getCacheEntry(channelUID)).thenReturn(passes);
        handler.handleCommand(channelUID, RefreshType.REFRESH);

        handler.dispose();
    }

    @Test
    void testCleanPassesReceivedAutomaticFirst() {
        handler.initialize();

        // Received FALSE noAutoPasses as first, doesn't matter what twoPasses is
        CleanPasses passes = new CleanPasses(false, null);
        ChannelGroupUID controlGroupUID = new ChannelGroupUID(thing.getUID(), CONTROL_GROUP_ID);
        ChannelUID channelUID = new ChannelUID(controlGroupUID, CHANNEL_CONTROL_CLEAN_PASSES);

        // No twoPasses value received
        passes.setTwoPass(null);
        doAnswer(invocation -> {
            assertEquals(StringType.valueOf(PASSES_AUTO), invocation.getArgument(1));
            return null;
        }).when(handler).updateState(any(ChannelUID.class), any(State.class));
        Mockito.when(handler.getCacheEntry(channelUID)).thenReturn(passes);
        handler.handleCommand(channelUID, RefreshType.REFRESH);

        // Received twoPasses
        passes.setTwoPass(false);
        doAnswer(invocation -> {
            assertEquals(StringType.valueOf(PASSES_AUTO), invocation.getArgument(1));
            return null;
        }).when(handler).updateState(any(ChannelUID.class), any(State.class));
        Mockito.when(handler.getCacheEntry(channelUID)).thenReturn(passes);
        handler.handleCommand(channelUID, RefreshType.REFRESH);

        passes.setTwoPass(true);
        doAnswer(invocation -> {
            assertEquals(StringType.valueOf(PASSES_AUTO), invocation.getArgument(1));
            return null;
        }).when(handler).updateState(any(ChannelUID.class), any(State.class));
        Mockito.when(handler.getCacheEntry(channelUID)).thenReturn(passes);
        handler.handleCommand(channelUID, RefreshType.REFRESH);

        handler.dispose();
    }

    @Test
    void testCleanPassesReceivedManualFirst() {
        handler.initialize();

        // Received TRUE noAutoPasses as first, inspect what twoPasses is
        CleanPasses passes = new CleanPasses(true, null);
        ChannelGroupUID controlGroupUID = new ChannelGroupUID(thing.getUID(), CONTROL_GROUP_ID);
        ChannelUID channelUID = new ChannelUID(controlGroupUID, CHANNEL_CONTROL_CLEAN_PASSES);

        // No twoPasses value received
        passes.setTwoPass(null);
        doAnswer(invocation -> {
            assertEquals(StringType.valueOf(PASSES_1), invocation.getArgument(1));
            return null;
        }).when(handler).updateState(any(ChannelUID.class), any(State.class));
        Mockito.when(handler.getCacheEntry(channelUID)).thenReturn(passes);
        handler.handleCommand(channelUID, RefreshType.REFRESH);

        // Received twoPasses
        passes.setTwoPass(false);
        doAnswer(invocation -> {
            assertEquals(StringType.valueOf(PASSES_1), invocation.getArgument(1));
            return null;
        }).when(handler).updateState(any(ChannelUID.class), any(State.class));
        Mockito.when(handler.getCacheEntry(channelUID)).thenReturn(passes);
        handler.handleCommand(channelUID, RefreshType.REFRESH);

        passes.setTwoPass(true);
        doAnswer(invocation -> {
            assertEquals(StringType.valueOf(PASSES_2), invocation.getArgument(1));
            return null;
        }).when(handler).updateState(any(ChannelUID.class), any(State.class));
        Mockito.when(handler.getCacheEntry(channelUID)).thenReturn(passes);
        handler.handleCommand(channelUID, RefreshType.REFRESH);

        handler.dispose();
    }

    @Test
    void testCleanPassesReceivedSinglePassFirst() {
        handler.initialize();

        // Received twoPasses first
        CleanPasses passes = new CleanPasses(null, false);
        ChannelGroupUID controlGroupUID = new ChannelGroupUID(thing.getUID(), CONTROL_GROUP_ID);
        ChannelUID channelUID = new ChannelUID(controlGroupUID, CHANNEL_CONTROL_CLEAN_PASSES);

        passes.setNoAutoPasses(null);
        doAnswer(invocation -> {
            assertEquals(StringType.valueOf(PASSES_1), invocation.getArgument(1));
            return null;
        }).when(handler).updateState(any(ChannelUID.class), any(State.class));
        Mockito.when(handler.getCacheEntry(channelUID)).thenReturn(passes);
        handler.handleCommand(channelUID, RefreshType.REFRESH);

        passes.setNoAutoPasses(false);
        doAnswer(invocation -> {
            assertEquals(StringType.valueOf(PASSES_AUTO), invocation.getArgument(1));
            return null;
        }).when(handler).updateState(any(ChannelUID.class), any(State.class));
        Mockito.when(handler.getCacheEntry(channelUID)).thenReturn(passes);
        handler.handleCommand(channelUID, RefreshType.REFRESH);

        passes.setNoAutoPasses(true);
        doAnswer(invocation -> {
            assertEquals(StringType.valueOf(PASSES_1), invocation.getArgument(1));
            return null;
        }).when(handler).updateState(any(ChannelUID.class), any(State.class));
        Mockito.when(handler.getCacheEntry(channelUID)).thenReturn(passes);
        handler.handleCommand(channelUID, RefreshType.REFRESH);

        handler.dispose();
    }

    @Test
    void testCleanPassesReceivedMultiplePassFirst() {
        handler.initialize();

        // Received twoPasses first
        CleanPasses passes = new CleanPasses(null, true);
        ChannelGroupUID controlGroupUID = new ChannelGroupUID(thing.getUID(), CONTROL_GROUP_ID);
        ChannelUID channelUID = new ChannelUID(controlGroupUID, CHANNEL_CONTROL_CLEAN_PASSES);

        passes.setNoAutoPasses(null);
        doAnswer(invocation -> {
            assertEquals(StringType.valueOf(PASSES_2), invocation.getArgument(1));
            return null;
        }).when(handler).updateState(any(ChannelUID.class), any(State.class));
        Mockito.when(handler.getCacheEntry(channelUID)).thenReturn(passes);
        handler.handleCommand(channelUID, RefreshType.REFRESH);

        passes.setNoAutoPasses(false);
        doAnswer(invocation -> {
            assertEquals(StringType.valueOf(PASSES_AUTO), invocation.getArgument(1));
            return null;
        }).when(handler).updateState(any(ChannelUID.class), any(State.class));
        Mockito.when(handler.getCacheEntry(channelUID)).thenReturn(passes);
        handler.handleCommand(channelUID, RefreshType.REFRESH);

        passes.setNoAutoPasses(true);
        doAnswer(invocation -> {
            assertEquals(StringType.valueOf(PASSES_2), invocation.getArgument(1));
            return null;
        }).when(handler).updateState(any(ChannelUID.class), any(State.class));
        Mockito.when(handler.getCacheEntry(channelUID)).thenReturn(passes);
        handler.handleCommand(channelUID, RefreshType.REFRESH);

        handler.dispose();
    }
}
