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
-define(DEFAULT_OAUTH_ENABLED, false).
-define(DEFAULT_KEY_NAME, dcos).

-define(REFRESH_INTERVAL, 100).
-define(REFRESH_MESSAGE, refresh).

-record(state, {running, launched}).

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
    schedule_refresh(),

    Running = dict:new(),
    Launched = [],

    {ok, #state{running=Running, launched=Launched}}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
handle_call({provision, StackName, Region},
            _From,
            #state{launched=Launched}=State) ->
    TemplateBody = template(Region),
    KeyName = ?DEFAULT_KEY_NAME,
    Capabilities = ?DEFAULT_CAPABILITIES,
    PublicSlaves = ?DEFAULT_PUBLIC_SLAVES,
    OAuthEnabled = ?DEFAULT_OAUTH_ENABLED,

    command(
        "aws cloudformation create-stack \\
                --stack-name ~s \\
                --template-body ~p \\
                --capabilities ~p \\
                --parameters \\
                    ParameterKey=KeyName,ParameterValue=~p \\
                    ParameterKey=OAuthEnabled,ParameterValue=~p \\
                    ParameterKey=PublicSlaveInstanceCount,ParameterValue=~p \\
                    ParameterKey=SlaveInstanceCount,ParameterValue=0",
           [StackName, TemplateBody, Capabilities, KeyName, OAuthEnabled, PublicSlaves]),

    %% Wait for cluster to be completed.
    wait(StackName, 'stack-create-complete'),

    {reply, ok, State#state{launched=Launched ++ [StackName]}};

handle_call({deprovision, StackName},
            _From,
            #state{running=Running0, launched=Launched0}=State) ->

    command("aws cloudformation delete-stack -stack-name ~s",
            [StackName]),

    Running = dict:erase(StackName, Running0),
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
handle_info(?REFRESH_MESSAGE, State) ->
    schedule_refresh(),
    {noreply, State};

handle_info({event, StackName, 'stack-create-complete'=Event},
            #state{running=Running0}=State) ->
    log("Event ~p received for ~p~n", [Event, StackName]),

    Output = command("aws cloudformation describe-stacks --stack-name ~p",
                     [StackName]),

    Running = try
                Config = jsx:decode(list_to_binary(Output), [return_maps]),
                dict:store(StackName, Config, Running0)
              catch
                  _:_ ->
                      Running0
              end,

    {noreply, State#state{running=Running}};

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
    log("Executing: ~s~n", [Command]),
    Output = os:cmd(Command),
    log("Output: ~s~n", [Output]),
    Output.

%% @private
schedule_refresh() ->
    erlang:send_after(?REFRESH_INTERVAL, self(), ?REFRESH_MESSAGE).

%% @private
%% TODO: check the return condition.
wait(StackName, Event) ->
    Self = self(),
    spawn(fun() ->
                command("aws cloudformation wait ~s --stack-name ~s",
                        [Event, StackName]),
                Self ! {event, StackName, Event}
          end).

%% @private
log(Message, Args) ->
    io:format(Message, Args).
