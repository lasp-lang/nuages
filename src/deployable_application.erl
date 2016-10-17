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

-module(deployable_application).
-author("Christopher S. Meiklejohn <christopher.meiklejohn@gmail.com").

-type id() :: string().

-callback specification(id()) -> #{}.

-export([ports/1,
         specification/9]).

ports(NumPorts) ->
    lists:map(fun(_) -> 0 end, lists:seq(1, NumPorts)).

specification(Identifier,
              Cpu,
              Memory,
              NumInstances,
              Constraints,
              DockerImage,
              NumPorts,
              Environment,
              Labels) ->
    #{
        wrap("acceptedResourceRoles") => [wrap("slave_public")],
        wrap("id") => wrap(Identifier),
        wrap("dependencies") => [],
        wrap("cpus") => Cpu,
        wrap("mem") => Memory,
        wrap("instances") => NumInstances,
        wrap("constraints") => Constraints,
        wrap("container") => #{
          wrap("type") => wrap("DOCKER"),
          wrap("docker") => #{
            wrap("image") => wrap(DockerImage),
            wrap("network") => wrap("HOST"),
            wrap("forcePullImage") => true,
            wrap("parameters") => [
              #{ wrap("key") => wrap("oom-kill-disable"),
                 wrap("value") => wrap("true") }
            ]
           }
        },
        wrap("ports") => ports(NumPorts),
        wrap("env") => Environment,
        wrap("labels") => Labels,
        wrap("healthChecks") => [
           #{
            wrap("path") => wrap("/api/health"),
            wrap("portIndex") => 0,
            wrap("protocol") => wrap("HTTP"),
            wrap("gracePeriodSeconds") => 300,
            wrap("intervalSeconds") => 60,
            wrap("timeoutSeconds") => 20,
            wrap("maxConsecutiveFailures") => 3,
            wrap("ignoreHttp1xx") => false
          }]
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
wrap(Term) ->
    list_to_binary(Term).
