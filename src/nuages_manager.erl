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

-module(nuages_manager).
-author("Christopher S. Meiklejohn <christopher.meiklejohn@gmail.com>").

-behaviour(gen_server).

%% API.
-export([start_link/0,
         provision/2,
         deprovision/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(DEFAULT_CAPABILITIES, "CAPABILITY_IAM").
-define(DEFAULT_PUBLIC_SLAVES, 1).

-record(state, {running=[], launched=[]}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Same as start_link([]).
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Provision a stack.
provision(StackName, Region) ->
    gen_server:call(?MODULE, {provision, StackName, Region}, infinity).

%% @doc Deprovision a stack.
deprovision(StackName) ->
    gen_server:call(?MODULE, {deprovision, StackName}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    {ok, #state{}}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
handle_call({provision, StackName, Region},
            _From,
            #state{launched=Launched}=State) ->
    TemplateBody = template(Region),
    Capabilities = ?DEFAULT_CAPABILITIES,
    PublicSlaves = ?DEFAULT_PUBLIC_SLAVES,

    command(
        "aws cloudformation create-stack \\
                --stack-name ~s \\
                --template-body ~p \\
                --capabilities ~p \\
                --parameters \\
                    ParameterKey=KeyName,ParameterValue=~p \\
                    ParameterKey=PublicSlaveInstanceCount,ParameterValue=~p \\
                    ParameterKey=SlaveInstanceCount,ParameterValue=0",
           [StackName, TemplateBody, Capabilities, StackName, PublicSlaves]),

    {reply, ok, State#state{launched=Launched ++ [StackName]}};

handle_call({deprovision, StackName},
            _From,
            #state{running=Running0, launched=Launched0}=State) ->

    command("aws cloudformation delete-stack -stack-name ~s",
            [StackName]),

    Running = lists:delete(StackName, Running0),
    Launched = lists:delete(StackName, Launched0),

    {reply, ok, State#state{running=Running, launched=Launched}};

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled messages: ~p", [Msg]),
    {reply, ok, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(Msg, State) ->
    lager:warning("Unhandled messages: ~p", [Msg]),
    {noreply, State}.

%% @private
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(Msg, State) ->
    lager:warning("Unhandled messages: ~p", [Msg]),
    {noreply, State}.

%% @private
-spec terminate(term(), #state{}) -> term().
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term() | {down, term()}, #state{}, term()) ->
    {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
template('us-west-2') ->
    "https://s3-us-west-2.amazonaws.com/downloads.dcos.io/dcos/stable/commit/e64024af95b62c632c90b9063ed06296fcf38ea5/cloudformation/multi-master.cloudformation.json".

%% @private
command(String, Args) ->
    Command = io_lib:format(String, Args),
    io:format("Executing: ~s~n", [Command]),
    Output = os:cmd(Command),
    io:format("Output: ~s~n", [Output]),
    ok.
