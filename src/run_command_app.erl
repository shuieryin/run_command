%%%-------------------------------------------------------------------
%%% @author Shuieryin
%%% @copyright (C) 2015, Shuieryin
%%% @doc
%%%
%%% Wechat mud application start.
%%%
%%% @end
%%% Created : 26. Aug 2015 11:04 AM
%%%-------------------------------------------------------------------
-module(run_command_app).

-behaviour(application).

%% Application callbacks
-export([
    start/2,
    stop/1
]).

-record(state, {}).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start application
%%
%% @end
%%--------------------------------------------------------------------
-spec start(StartType, StartArgs) -> Return when
    StartType :: application:start_type(),
    StartArgs :: term(), % generic term
    Return :: {ok, pid(), State :: #state{}}.
start(normal, [_AppNameStr] = StartArgs) ->
    {ok, Pid} = run_command_sup:start_link(StartArgs),
    ok = elib:show_errors(20),
    {ok, Pid, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% Stop application
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(State) -> ok when
    State :: #state{}.
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================