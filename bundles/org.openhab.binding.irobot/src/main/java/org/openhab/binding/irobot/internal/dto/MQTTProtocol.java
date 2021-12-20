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
package org.openhab.binding.irobot.internal.dto;

import java.util.ArrayList;
import java.util.List;

import com.google.gson.annotations.SerializedName;

/**
 * iRobot MQTT protocol messages
 *
 * @author Pavel Fedin - Initial contribution
 * @author Florian Binder - Added CleanRoomsRequest
 *
 */
public class MQTTProtocol {
    public interface Request {
        public String getTopic();
    }

    public static class CleanRoomsRequest extends CommandRequest {
        public int ordered;
        @SerializedName("pmap_id")
        public String pmapId;
        @SerializedName("user_pmapv_id")
        public String userPmapvId;
        public List<Region> regions;

        public CleanRoomsRequest(String cmd, String mapId, String[] pregions, String[] types, String userPmapvId) {
            super(cmd);
            ordered = 1;
            pmapId = mapId;
            this.userPmapvId = userPmapvId;

            regions = new ArrayList<Region>();
            for (int i = 0; (i < pregions.length) && (i < types.length); i++) {
                regions.add(new Region(pregions[i], types[i]));
            }
        }

        public static class Region {
            @SerializedName("region_id")
            public String regionId;
            public String type;

            public Region(String id, String type) {
                this.regionId = id;
                this.type = type;
            }
        }
    }

    public static class CommandRequest implements Request {
        public String command;
        public long time;
        public String initiator;

        public CommandRequest(String cmd) {
            command = cmd;
            time = System.currentTimeMillis() / 1000;
            initiator = "openhab";
        }

        @Override
        public String getTopic() {
            return "cmd";
        }
    }

    public static class StateValue {
        // Just some common type, nothing to do here
        protected StateValue() {
        }
    }
};
