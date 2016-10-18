%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Christopher Meiklejohn.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(lasp).
-author("Christopher S. Meiklejohn <christopher.meiklejohn@gmail.com>").

-behaviour(deployable_application).

-export([specification/1]).

specification(Identifier) ->
    Cpu = 1,
    Memory = 1024,
    NumInstances = 1,
    DockerImage = "cmeiklejohn/lasp-dev",
    Ports = [0, 0],

    Constraints = [],

    Environment = #{
        wrap("LASP_BRANCH") => env_wrap("$LASP_BRANCH"),
        wrap("AD_COUNTER_SIM_SERVER") => env_wrap("true"),
        wrap("DCOS") => env_wrap("$DCOS"),
        wrap("TOKEN") => env_wrap("$TOKEN"),
        wrap("PEER_SERVICE") => env_wrap("$PEER_SERVICE"),
        wrap("MODE") => env_wrap("$MODE"),
        wrap("BROADCAST") => env_wrap("$BROADCAST"),
        wrap("SIMULATION") => env_wrap("$SIMULATION"),
        wrap("EVAL_ID") => env_wrap("$EVAL_ID"),
        wrap("EVAL_TIMESTAMP") => env_wrap("$EVAL_TIMESTAMP"),
        wrap("CLIENT_NUMBER") => env_wrap("$CLIENT_NUMBER"),
        wrap("HEAVY_CLIENTS") => env_wrap("$HEAVY_CLIENTS"),
        wrap("REACTIVE_SERVER") => env_wrap("$REACTIVE_SERVER"),
        wrap("PARTITION_PROBABILITY") => env_wrap("$PARTITION_PROBABILITY"),
        wrap("IMPRESSION_VELOCITY") => env_wrap("$IMPRESSION_VELOCITY"),
        wrap("AAE_INTERVAL") => env_wrap("$AAE_INTERVAL"),
        wrap("DELTA_INTERVAL") => env_wrap("$DELTA_INTERVAL"),
        wrap("INSTRUMENTATION") => env_wrap("$INSTRUMENTATION"),
        wrap("LOGS") => env_wrap("$LOGS"),
        wrap("EXTENDED_LOGGING") => env_wrap("$EXTENDED_LOGGING"),
        wrap("MAILBOX_LOGGING") => env_wrap("$MAILBOX_LOGGING"),
        wrap("AWS_ACCESS_KEY_ID") => env_wrap("$AWS_ACCESS_KEY_ID"),
        wrap("AWS_SECRET_ACCESS_KEY") => env_wrap("$AWS_SECRET_ACCESS_KEY")
    },

    Labels = #{
        wrap("HAPROXY_GROUP") => wrap("external"),
        wrap("HAPROXY_0_VHOST") => env_wrap("$ELB_HOST")
    },

    Specification = deployable_application:specification(Identifier,
                                                         Cpu,
                                                         Memory,
                                                         NumInstances,
                                                         Constraints,
                                                         DockerImage,
                                                         Ports,
                                                         Environment,
                                                         Labels),

    Specification.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
env_wrap(Term) ->
    V = case os:getenv(Term) of
        false ->
            "false";
        Value ->
            Value
    end,
    list_to_binary(V).

%% @private
wrap(Term) ->
    list_to_binary(Term).
