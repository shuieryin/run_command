


%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2017, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 06. Jan 2017 10:19 PM
%%%-------------------------------------------------------------------
-module(run_command_server).
-author("shuieryin").

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    handle_command/2,
    handle_command/1,
    acceptor/2,
    collect_command_output/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    format_status/2
]).

-define(SERVER, ?MODULE).
-define(EMPTY_CONTENT, <<>>).

-record(state, {
    socket :: gen_tcp:socket()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> gen:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(Args) ->
    {ok, State} |
    {ok, State, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    Args :: term(),
    State :: #state{},
    Reason :: term(). % generic term
init([]) ->
    io:format("~p starting...", [?MODULE]),

    process_flag(trap_exit, true),

    {ok, ListenSocket} = ranch_tcp:listen([{port, 12346}, {keepalive, true}]),

    spawn(?MODULE, acceptor, [ListenSocket, true]),

    io:format("started~n~n"),

    {ok, #state{
        socket = ListenSocket
    }}.

%%--------------------------------------------------------------------
%% @doc
%% Acceptor
%%
%% @end
%%--------------------------------------------------------------------
-spec acceptor(gen_tcp:socket(), boolean()) -> no_return().
acceptor(ListenSocket, IsInit) ->
    Self = self(),
    case IsInit of
        true ->
            Self ! {start_accept, Self};
        false ->
            ok
    end,

    receive
        {start_accept, Self} ->
            io:format("pending accept socket ~p~n", [ListenSocket]),
            {ok, AcceptSocket} = ranch_tcp:accept(ListenSocket, infinity),
            inet:setopts(AcceptSocket, [{active, true}]),
            io:format("socket ~p accepted~n", [AcceptSocket]),
            apply(?MODULE, handle_command, [AcceptSocket, Self]),
            acceptor(ListenSocket, false)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Handle comment from incoming socket
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_command(gen_tcp:socket(), pid()) -> no_return().
handle_command(Socket, Acceptor) ->
    receive
        {tcp, Socket, <<0, _Type, "close", _Rest/binary>>} ->
            io:format("closing socket:~p~n", [Socket]),
            ranch_tcp:send(Socket, encode_response(<<"closed">>)),
            ranch_tcp:close(Socket),
            Acceptor ! {start_accept, Acceptor};
        {tcp, Socket, <<_Ind, _Type, MsgBin/binary>>} ->
            Msg = base64:decode(MsgBin),
            io:format("Msg:~p~n", [Msg]),
            elib:cmd(binary_to_list(Msg),
                fun(RawOutputBin) ->
                    io:format("~p~n", [RawOutputBin]),
                    ranch_tcp:send(Socket, encode_response(RawOutputBin))
                end, []
            ),
            handle_command(Socket, Acceptor)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Handle comment from http
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_command(Req :: cowboy_req:req()) -> {iodata(), cowboy_req:req()}.
handle_command(Req) ->
    case cowboy_req:qs(Req) of
        ?EMPTY_CONTENT ->
            ok;
        HeaderParams ->
            error_logger:info_msg("==========HeaderParams:~n~p~n", [HeaderParams])
    end,

    {ok, RawCommandBin, UpdatedReq} = cowboy_req:read_body(Req),
    ReturnMessage =
        case RawCommandBin of
            ?EMPTY_CONTENT ->
                ?EMPTY_CONTENT;
            _HasContent ->
                Self = self(),
                CollectOutputPid = spawn(?MODULE, collect_command_output, [Self, []]),
                #{
                    <<"command">> := CommandBin
                } = jsx:decode(RawCommandBin, [return_maps]),
                elib:cmd(binary_to_list(CommandBin),
                    fun(RawOutputBin) ->
                        CollectOutputPid ! {collect, CollectOutputPid, RawOutputBin},
                        io:format("~p~n", [RawOutputBin])
                    end, []
                ),

                receive
                    {output, CollectOutputPid, OutputList} ->
                        jsx:encode(OutputList)
                end
        end,
    {ReturnMessage, UpdatedReq}.

%%--------------------------------------------------------------------
%% @doc
%% Collect all outputs from elib cmd
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_command_output(pid(), [binary()]) -> no_return().
collect_command_output(ReturnPid, AccOutputList) ->
    Self = self(),
    receive
        {collect, Self, <<"done\n">> = Done} ->
            ReturnPid ! {output, Self, lists:reverse([Done | AccOutputList])};
        {collect, Self, OutputLine} ->
            collect_command_output(ReturnPid, [OutputLine | AccOutputList])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request, From, State) ->
    {reply, Reply, NewState} |
    {reply, Reply, NewState, timeout() | hibernate} |
    {noreply, NewState} |
    {noreply, NewState, timeout() | hibernate} |
    {stop, Reason, Reply, NewState} |
    {stop, Reason, NewState} when

    Request :: term(),  % generic term
    Reply :: ok,

    From :: {pid(), Tag :: term()}, % generic term
    State :: #state{},
    NewState :: State,
    Reason :: term(). % generic term
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request, State) ->
    {noreply, NewState} |
    {noreply, NewState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    Request :: term() | stop, % generic term
    State :: #state{},
    NewState :: State,
    Reason :: term(). % generic term
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info | timeout(), State) ->
    {noreply, NewState} |
    {noreply, NewState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    Info :: term(), % generic term
    State :: #state{},
    NewState :: State,
    Reason :: term(). % generic term
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, State) -> ok when
    Reason :: (normal | shutdown | {shutdown, term()} | term()), % generic term
    State :: #state{}.
terminate(_Reason, #state{
    socket = Socket
}) ->
    ranch_tcp:send(Socket, <<"closed\n">>),
    ranch_tcp:close(Socket),
    rb:stop(),
    os:cmd("redis-cli shutdown"),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn, State, Extra) ->
    {ok, NewState} |
    {error, Reason} when

    OldVsn :: term() | {down, term()}, % generic term
    State :: #state{},
    Extra :: term(), % generic term
    NewState :: State,
    Reason :: term(). % generic term
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is useful for customising the form and
%% appearance of the gen_server status for these cases.
%%
%% @spec format_status(Opt, StatusData) -> Status
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt, StatusData) -> Status when
    Opt :: 'normal' | 'terminate',
    StatusData :: [PDict | State],
    PDict :: [{Key :: term(), Value :: term()}], % generic term
    State :: #state{},
    Status :: term(). % generic term
format_status(Opt, StatusData) ->
    gen_server:format_status(Opt, StatusData).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Encode response message.
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_response(RawBin) -> EncodedBin when
    RawBin :: binary(),
    EncodedBin :: binary().
encode_response(RawBin) ->
    EcnodedBin = base64:encode(RawBin),
    <<EcnodedBin/binary, "\n">>.