-module(client_srvr).
%% Module for handling the client connections

-behaviour(gen_server).

-include("../include/cmds.hrl").

%% API for client_router
-export([forward/3]).

%% gen_server exports and harness startup
-export([start/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state,
        { sock      :: gen_tcp:socket()
        , game_info :: game_info()
        , csm       :: client_sm:csm()
        }).

-record(game_info,
        { user_info :: match_maker:user_info()
        , id        :: common:game_id()
        }).

%% As defined in match_maker
-record(user_info,
        { name :: string()
        }).


-type game_info() :: #game_info{}.

%% API
-spec forward(pid(), common:cmd(), common:request()) -> ok.
forward(Pid, Cmd, Request) ->
    gen_server:cast(Pid, {forward, Cmd, Request}).

%% Startup
start([ListenSock]) ->
    gen_server:start(?MODULE, [ListenSock], []).

start_link([ListenSock]) ->
    gen_server:start_link(?MODULE, [ListenSock], []).

init([ListenSock]) ->
    {ok, Sock} = gen_tcp:accept(ListenSock),
    spawn_link(fun client_sup:start_socket/0), %% Replace with listening socket
    {ok, Csm} = client_sm:start_link(),
    {ok, #state{sock = Sock, csm = Csm}}.

%% TCP Socket handling
handle_info({tcp, Sock, <<Cmd, Bin/binary>>}, #state{sock = Sock} = State) ->
    {ok, State1} = case client_sm:validate_cmd(State#state.csm, Cmd, Bin) of
                     {valid, general} ->
                         do_handle_general(Cmd, Bin, State);
                     {valid, lobby} ->
                         do_handle_lobby(Cmd, Bin, State);
                     {valid, game} ->
                         do_handle_game(Cmd, Bin, State);
                     invalid ->
                         send_to_client(?INVALID, <<>>, Sock),
                         {ok, State}
                   end,
    inet:setopts(Sock, [{active, once}]),
    {noreply, State1};
handle_info({tcp_closed, Sock}, #state{sock = Sock} = State) ->
    {stop, normal, State};
handle_info({tcp_error, Sock}, #state{sock = Sock} = State) ->
    {stop, normal, State}.


%% Call catch-all
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast({forward, ?STARTED, <<GameID:32>>}, State) ->
    ok = do_forward(?STARTED, <<GameID:32>>, State),
    GameInfo = State#state.game_info,
    {noreply, State#state{game_info = GameInfo#game_info{id = GameID}}};
handle_cast({forward, Cmd, Request}, State) ->
    ok = do_forward(Cmd, Request, State),
    {noreply, State};
%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.

%% Terminate catch-all
terminate(_Reason, #state{sock = Sock} = _State) ->
    try
        gen_tcp:close(Sock),
        ok
    catch
        _:_ ->
            ok
    end.


-spec send_to_client(common:cmd(), common:request(), gen_tcp:socket()) -> ok.
send_to_client(Cmd, Request, Sock) when byte_size(Request) < 254 ->
    Bin = iolist_to_binary([Cmd, Request]),
    gen_tcp:send(Sock, Bin).

-spec do_forward(common:cmd(), common:request(), State :: term()) -> ok.
do_forward(Cmd, <<>>, State) ->
    GameInfo = State#state.game_info,
    %% No need to bother the client we can just say that we are ready
    relayer:respond(GameInfo#game_info.id, ready);
do_forward(Cmd, Request, State) ->
    %% Update the csm to know the current cmd for validation on client response
    ok = client_sm:update_cmd(State#state.csm, Cmd),
    %% Send the request to the client
    send_to_client(Cmd, Request, State#state.sock).


%% Handle functions that can assume that the binary inputs are valid
-spec do_handle_general(common:cmd(), common:response(), State :: term()) -> ok.
do_handle_general(_Cmd, _Bin, State) ->
    {ok, State}.

-spec do_handle_lobby(common:cmd(), common:response(), State :: term()) -> ok.
do_handle_lobby(?LOGIN, Bin, #state{sock = Sock} = State) ->
    Username = binary_to_list(Bin),
    UserInfo = #user_info{name = Username},
    GameInfo = #game_info{user_info = UserInfo},
    send_to_client(?OK, <<>>, Sock),
    {ok, State#state{game_info = GameInfo}};
do_handle_lobby(?QUEUE, ConfigBin, #state{} = State) ->
    UserInfo = (State#state.game_info)#game_info.user_info,
    match_maker:queue(UserInfo, ConfigBin),
    {ok, State};
do_handle_lobby(_Cmd, _Bin, State) ->
    {ok, State}.

-spec do_handle_game(common:cmd(), common:response(), State :: term()) -> ok.
do_handle_game(Cmd, Response, #state{game_info = GameInfo} = State) ->
    ok = client_sm:update_cmd(State#state.csm, Cmd),
    GameID = GameInfo#game_info.id,
    relayer:respond(GameID, Response),
    {ok, State}.
