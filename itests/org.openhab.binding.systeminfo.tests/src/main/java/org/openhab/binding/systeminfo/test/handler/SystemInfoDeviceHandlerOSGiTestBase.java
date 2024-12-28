/**
 * Copyright (c) 2010-2024 Contributors to the openHAB project
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
package org.openhab.binding.systeminfo.test.handler;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.openhab.binding.systeminfo.internal.SystemInfoBindingConstants.PRIORITY_PARAMETER;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.openhab.core.config.core.Configuration;
import org.openhab.core.thing.Bridge;
import org.openhab.core.thing.ChannelUID;
import org.openhab.core.thing.Thing;
import org.openhab.core.thing.ThingStatus;
import org.openhab.core.thing.ThingStatusInfo;
import org.openhab.core.thing.ThingTypeUID;
import org.openhab.core.thing.ThingUID;
import org.openhab.core.thing.binding.ThingHandler;
import org.openhab.core.thing.binding.builder.ChannelBuilder;
import org.openhab.core.thing.binding.builder.ThingBuilder;
import org.openhab.core.thing.type.ChannelKind;
import org.openhab.core.thing.type.ChannelTypeUID;

/**
 * Basis class for OSGi device tests
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class SystemInfoDeviceHandlerOSGiTestBase extends SystemInfoOSGiTestBase {

    private @Nullable Thing thing;

    @BeforeEach
    @Override
    public void initialize() {
        super.initialize();
    }

    @AfterEach
    @Override
    public void dispose() {
        final Thing thing = this.thing;
        this.thing = null;

        if (thing != null) {
            // Remove the systeminfo thing. The handler will also be disposed automatically
            final Thing removedThing = thingRegistry.forceRemove(thing.getUID());
            if (removedThing == null) {
                throw new AssertionError("The device thing cannot be deleted");
            }

            managedThingProvider.remove(removedThing.getUID());

            final ThingHandler handler = removedThing.getHandler();
            assertThat(handler, is(nullValue()));
        }

        super.dispose();
    }

    protected Thing initializeThingWithChannel(final ThingUID thingUID, final ThingTypeUID thingTypeUID,
            final Configuration thingConfig, final ChannelUID channelUID, final ChannelTypeUID channelTypeUID,
            final String acceptedItemType) {
        initializeThing(configuration, null, "", DEFAULT_CHANNEL_TEST_PRIORITY);

        final Bridge bridge = systemInfoBridge;
        if (bridge == null) {
            throw new AssertionError("Bridge is null");
        }

        final ThingBuilder thingBuilder = ThingBuilder.create(thingTypeUID, thingUID);
        thingBuilder.withBridge(bridge.getUID()).withConfiguration(thingConfig);

        final ChannelBuilder channelBuilder = ChannelBuilder.create(channelUID, acceptedItemType);
        channelBuilder.withType(channelTypeUID).withKind(ChannelKind.STATE);

        Configuration channelConfiguration = new Configuration();
        channelConfiguration.put(PRIORITY_PARAMETER, DEFAULT_CHANNEL_TEST_PRIORITY);
        channelBuilder.withConfiguration(channelConfiguration);
        thingBuilder.withChannel(channelBuilder.build());

        final Thing thing = thingBuilder.build();
        assertThat(thing, is(notNullValue()));
        managedThingProvider.add(thing);

        final ThingHandler handler = thing.getHandler();
        if (handler == null) {
            throw new AssertionError("Device handler is null");
        }
        handler.initialize();

        final Thing createdThing = handler.getThing();
        assertThat(createdThing, is(instanceOf(Thing.class)));

        waitForAssert(() -> {
            final ThingStatusInfo statusInfo = createdThing.getStatusInfo();
            assertThat(String.format("Thing status detail is %s with description %s", statusInfo.getStatusDetail(),
                    statusInfo.getDescription()), createdThing.getStatus(), is(equalTo(ThingStatus.ONLINE)));
        });

        this.thing = createdThing;
        return createdThing;
    }
}
