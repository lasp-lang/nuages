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

-module(antidote_development).
-author("Christopher S. Meiklejohn <christopher.meiklejohn@gmail.com>").

-behaviour(deployable_application).

-export([specification/1]).

specification(Identifier) ->
    Cpu = 1,
    Memory = 1024,
    NumInstances = 1,
    DockerImage = "cmeiklejohn/antidote-mesos-dev",
    NumPorts = 4,

    Constraints = [[wrap("hostname"), wrap("UNIQUE")]],

    Environment = #{},

    Labels = #{},

    Specification = deployable_application:specification(Identifier,
                                                         Cpu,
                                                         Memory,
                                                         NumInstances,
                                                         Constraints,
                                                         DockerImage,
                                                         NumPorts,
                                                         Environment,
                                                         Labels),

    Specification.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
wrap(Term) ->
    list_to_binary(Term).
