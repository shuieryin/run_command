%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jan 2017 10:57 PM
%%%-------------------------------------------------------------------
-module(execute_commands).
-author("shuieryin").

-behaviour(ranch_protocol).

%% API
-export([
    start_link/4,
    init/4
]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    io:format("~p starting...", [?MODULE]),
    ok = ranch:accept_ack(Ref),

    io:format("started~n~n"),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, Data} ->
            io:format("received data:~p~n", [Data]),
            Transport:send(Socket, Data),
            loop(Socket, Transport);
        _ ->
            ok = Transport:close(Socket)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Comment starts here
%%
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================