-module(harness).

%% Harness around the interacting system that only forwards the logic aspects
%% of the external program. All logging of the external program is sent through
%% its stdout and into porter.

-behaviour(gen_server).

%% API to send a message to the Haskell program
-export([respond/2]).

%% gen_server exports and harness startup
-export([start/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state,
        { l_sock :: gen_tcp:socket()
        , sock :: gen_tcp:socket()
        }).

-type port_id()    :: integer().

-define(NAME, ?MODULE).

%% API
-spec respond(common:game_id(), iolist()) -> ok.
respond(GameID, Bin) when is_integer(GameID) ->
    gen_server:cast(?NAME, {respond, GameID, Bin}).

%% Startup
-spec start(port_id()) -> gen_server:start_ret().
start(Port) ->
        gen_server:start({local, ?NAME}, ?MODULE, [Port], []).

-spec start_link(port_id()) -> gen_server:start_ret().
start_link(Port) ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [Port], []).

%% gen_server exports
init([Port]) ->
    gen_server:cast(?NAME, init),
    %% We can just let our things crash in the unlikely event that someone
    %% casts before we have processed our own init message with the undefined
    %% state. We are required to init so that the supervisor can add start up
    %% the other pids.
    {ok, {init, Port}}.

%% Manual exiting
%% TCP Socket handling
handle_info({tcp, Sock, Bin}, #state{sock = Sock} = State) ->
    ok = do_forward(Bin),
    {noreply, State};
handle_info({tcp_closed, Sock}, #state{sock = Sock} = State) ->
    {stop, normal, State};
handle_info({tcp_error, Sock}, #state{sock = Sock} = State) ->
    {stop, normal, State};
handle_info(_Exit, State) ->
    {stop, normal, State}.

%% Call catch-all
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast(init, {init, Port}) ->
    {ok, State} = do_init(Port),
    {noreply, State};
handle_cast({respond, GameID, Bin}, State) ->
    ok = do_respond(GameID, Bin, State#state.sock),
    {noreply, State};
%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.

%% Terminate catch-all
terminate(_Reason, #state{sock = Sock, l_sock = ListenSock} = _State) ->
    try
        gen_tcp:close(Sock),
        gen_tcp:close(ListenSock),
        ok
    catch
        _:_ ->
            ok
    end.

%% Helper functions
do_init(Port) ->
    {ok, ListenSock}
        = gen_tcp:listen(Port,
                         [ binary
                         , {packet, 1}
                         , {reuseaddr, true}
                         , {active, true}
                         ]),
    {ok, Sock} = gen_tcp:accept(ListenSock),
    {ok, #state{l_sock = ListenSock, sock = Sock}}.

-spec do_forward(common:request()) -> ok.
do_forward(Bin) ->
    <<GameID:32, Cmd, Request/binary>> = Bin,
    client_router:forward(GameID, Cmd, Request).

-spec do_respond(common:game_id(), common:response(), gen_tcp:socket()) -> ok.
do_respond(GameID, Response, Sock) when is_binary(Response) ->
    Bin = <<GameID:32, Response/binary>>,
    gen_tcp:send(Sock, Bin);
do_respond(GameID, Response, Sock) ->
    Bin = iolist_to_binary([<<GameID:32>>, Response]),
    gen_tcp:send(Sock, Bin).
