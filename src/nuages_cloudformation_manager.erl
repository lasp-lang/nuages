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

-module(nuages_cloudformation_manager).
-author("Christopher S. Meiklejohn <christopher.meiklejohn@gmail.com>").

-behaviour(gen_server).

%% API.
-export([start_link/0,
         provision/2,
         deprovision/2,
         deploy/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(DEFAULT_CAPABILITIES, "CAPABILITY_IAM").
-define(DEFAULT_PUBLIC_SLAVES, 2).
-define(DEFAULT_OAUTH_ENABLED, false).
-define(DEFAULT_KEY_NAME, dcos).

-define(REFRESH_INTERVAL, 100).
-define(REFRESH_MESSAGE, refresh).

-record(state, {stacks}).
-record(deployment, {region, configuration, applications}).

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
deprovision(StackName, Region) ->
    gen_server:call(?MODULE, {deprovision, StackName, Region}, infinity).

%% @doc Deploy an application.
deploy(Identifier, Application) ->
    gen_server:call(?MODULE, {deploy, Identifier, Application}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    schedule_refresh(),
    Stacks = dict:new(),
    {ok, #state{stacks=Stacks}}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
handle_call({deploy, Identifier, Application},
            _From,
            #state{stacks=Stacks0}=State) ->

    %% Get application specification.
    Specification = Application:specification(Identifier),
    EncodedSpec = jsx:encode(Specification),

    log("Specification: ~p~n", [EncodedSpec]),

    FoldFun = fun(StackName,
                  #deployment{configuration=Configuration, applications=Applications0} = Deployment,
                  Stacks1) ->
                    deploy_to_stack(Identifier,
                                    Configuration,
                                    EncodedSpec),
                    Applications = Applications0 ++ [Identifier],
                    lager:info("Stack ~p now has applications: ~p",
                               [StackName, Applications]),
                    dict:store(StackName,
                               Deployment#deployment{applications=Applications},
                               Stacks1)
              end,
    Stacks = dict:fold(FoldFun, Stacks0, Stacks0),

    {reply, ok, State#state{stacks=Stacks}};

handle_call({provision, StackName, Region},
            _From,
            State) ->
    TemplateBody = template(Region),
    KeyName = ?DEFAULT_KEY_NAME,
    Capabilities = ?DEFAULT_CAPABILITIES,
    PublicSlaves = ?DEFAULT_PUBLIC_SLAVES,
    OAuthEnabled = ?DEFAULT_OAUTH_ENABLED,

    command(
        "aws --region ~p cloudformation create-stack \\
                --stack-name ~s \\
                --template-body ~p \\
                --capabilities ~p \\
                --parameters \\
                    ParameterKey=KeyName,ParameterValue=~p \\
                    ParameterKey=OAuthEnabled,ParameterValue=~p \\
                    ParameterKey=PublicSlaveInstanceCount,ParameterValue=~p \\
                    ParameterKey=SlaveInstanceCount,ParameterValue=0",
           [Region, StackName, TemplateBody, Capabilities, KeyName, OAuthEnabled, PublicSlaves]),

    %% Wait for cluster to be completed.
    wait(StackName, Region, 'stack-create-complete'),

    {reply, ok, State};

handle_call({deprovision, StackName, Region},
            _From,
            #state{stacks=Stacks0}=State) ->

    command("aws --region ~p cloudformation delete-stack -stack-name ~s",
            [Region, StackName]),

    Stacks = dict:erase(StackName, Stacks0),

    {reply, ok, State#state{stacks=Stacks}};

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

handle_info({event, StackName, Region, 'stack-create-complete'=Event},
            #state{stacks=Stacks0}=State) ->
    log("Event ~p received for ~p~n", [Event, StackName]),

    Output = command("aws --region ~p cloudformation describe-stacks --stack-name ~p",
                     [Region, StackName]),

    Stacks = try
                Configuration = jsx:decode(list_to_binary(Output), [return_maps]),
                dict:store(StackName,
                           #deployment{region=Region,
                                       configuration=Configuration,
                                       applications=[]},
                           Stacks0)
              catch
                  _:_ ->
                      Stacks0
              end,

    {noreply, State#state{stacks=Stacks}};

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
template('us-west-1') ->
    "https://s3-us-west-2.amazonaws.com/downloads.dcos.io/dcos/stable/commit/e665123df0dbb19adacaefe47d16a3de144d5733/cloudformation/multi-master.cloudformation.json";
template('us-west-2') ->
    "https://s3-us-west-2.amazonaws.com/downloads.dcos.io/dcos/stable/commit/e64024af95b62c632c90b9063ed06296fcf38ea5/cloudformation/multi-master.cloudformation.json";
template('us-east-1') ->
    "https://s3-us-west-2.amazonaws.com/downloads.dcos.io/dcos/stable/commit/e665123df0dbb19adacaefe47d16a3de144d5733/cloudformation/multi-master.cloudformation.json";
template('sa-east-1') ->
    "https://s3-us-west-2.amazonaws.com/downloads.dcos.io/dcos/stable/commit/e665123df0dbb19adacaefe47d16a3de144d5733/cloudformation/multi-master.cloudformation.json";
template('eu-west-1') ->
    "https://s3-us-west-2.amazonaws.com/downloads.dcos.io/dcos/stable/commit/e665123df0dbb19adacaefe47d16a3de144d5733/cloudformation/multi-master.cloudformation.json";
template('eu-central-1') ->
    "https://s3-us-west-2.amazonaws.com/downloads.dcos.io/dcos/stable/commit/e665123df0dbb19adacaefe47d16a3de144d5733/cloudformation/multi-master.cloudformation.json";
template('ap-northeast-1') ->
    "https://s3-us-west-2.amazonaws.com/downloads.dcos.io/dcos/stable/commit/e665123df0dbb19adacaefe47d16a3de144d5733/cloudformation/multi-master.cloudformation.json";
template('ap-southeast-1') ->
    "https://s3-us-west-2.amazonaws.com/downloads.dcos.io/dcos/stable/commit/e665123df0dbb19adacaefe47d16a3de144d5733/cloudformation/multi-master.cloudformation.json";
template('ap-southeast-2') ->
    "https://s3-us-west-2.amazonaws.com/downloads.dcos.io/dcos/stable/commit/e665123df0dbb19adacaefe47d16a3de144d5733/cloudformation/multi-master.cloudformation.json";
template(_) ->
    undefined.

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
wait(StackName, Region, Event) ->
    Self = self(),
    spawn(fun() ->
                command("aws --region ~p cloudformation wait ~s --stack-name ~s",
                        [Region, Event, StackName]),
                Self ! {event, StackName, Region, Event}
          end).

%% @private
log(Message, Args) ->
    lager:info(Message, Args).

%% @private
deploy_to_stack(Identifier, Configuration, Specification) ->
    %% Get the endpoint from the cluster configuraiton.
    #{<<"Stacks">> := [Stack]} = Configuration,
    #{<<"Outputs">> := Outputs} = Stack,
    Mesos = lists:foldl(fun(#{<<"OutputKey">> := OutputKey,
                      <<"OutputValue">> := OutputValue},
                    DnsAddress) ->
                        case OutputKey of
                            <<"DnsAddress">> ->
                                OutputValue;
                            _ ->
                                DnsAddress
                        end
                end, undefined, Outputs),
    log("~p~n", [Mesos]),

    %% Write the configuration to a temporary file.
    Filename = "/tmp/configuration.json",
    ok = file:write_file(Filename, Specification),

    %% Remove the application first.
    command("curl -k -H 'Content-type: application/json' \\
                  -X DELETE ~s/service/marathon/v2/apps/~s",
            [Mesos, Identifier]),

    %% Deploy the application.
    command("curl -k -H 'Content-type: application/json' \\
                  -X POST -d @~s ~s/service/marathon/v2/apps?force=true",
            [Filename, Mesos]),

    ok.
