/**
 * Copyright (c) 2021- Alexander Falkenstern
 *
 * License: https://www.gnu.org/licenses/gpl-3.0.txt
 */
package org.openhab.binding.irobot.internal.handler;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.openhab.binding.irobot.internal.IRobotChannelContentProvider;
import org.openhab.core.thing.Thing;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.*;

/**
 * The {@link RoombaSModelsHandler} is responsible for handling commands, which are
 * sent to one of the channels.
 *
 * @author Alexander Falkenstern - Initial contribution
 */
@NonNullByDefault
public class RoombaSModelsHandler extends RoombaCommonHandler {
    private final Logger logger = LoggerFactory.getLogger(RoombaSModelsHandler.class);

    public RoombaSModelsHandler(Thing thing, IRobotChannelContentProvider channelContentProvider) {
        super(thing, channelContentProvider);
    }
}
