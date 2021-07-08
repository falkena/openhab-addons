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

import java.util.Arrays;

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

    public static class DeltaRequest implements Request {
        public StateValue state;

        public DeltaRequest(StateValue state) {
            this.state = state;
        }

        @Override
        public String getTopic() {
            return "delta";
        }
    }

    public static class StateValue {
        // Just some common type, nothing to do here
        protected StateValue() {
        }
    }

    public static class SubModSwVer {
        public String nav;
        public String mob;
        public String pwr;
        public String sft;
        public String mobBtl;
        public String linux;
        public String con;
    }

    // "reported" messages never contain the full state, only a part.
    // Therefore all the fields in this class are nullable
    public static class GenericState extends StateValue {
        // "softwareVer":"v2.4.6-3"
        public String softwareVer;
        // "navSwVer":"01.12.01#1"
        public String navSwVer;
        // "wifiSwVer":"20992"
        public String wifiSwVer;
        // "mobilityVer":"5806"
        public String mobilityVer;
        // "bootloaderVer":"4042"
        public String bootloaderVer;
        // "umiVer":"6",
        public String umiVer;
        // "sku":"R981040"
        public String sku;
        // "batteryType":"lith"
        public String batteryType;
        // Used by i7:
        // "subModSwVer":{
        // "nav": "lewis-nav+3.2.4-EPMF+build-HEAD-7834b608797+12", "mob":"3.2.4-XX+build-HEAD-7834b608797+12",
        // "pwr": "0.5.0+build-HEAD-7834b608797+12",
        // "sft":"1.1.0+Lewis-Builds/Lewis-Certified-Safety/lewis-safety-bbbe81f2c82+21",
        // "mobBtl": "4.2", "linux":"linux+2.1.6_lock-1+lewis-release-rt419+12",
        // "con":"2.1.6-tags/release-2.1.6@c6b6585a/build"}
        public SubModSwVer subModSwVer;
    }

    // Data comes as JSON string: {"state":{"reported":<Actual content here>}}
    // or: {"state":{"desired":<Some content here>}}
    // Of the second form i've so far observed only: {"state":{"desired":{"echo":null}}}
    // I don't know what it is, so let's ignore it.
    public static class ReportedState {
        public GenericState reported;
    }

    public static class StateMessage {
        public ReportedState state;
    }
};
