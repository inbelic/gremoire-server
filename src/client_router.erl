-module(client_router).

%% This server implements the logic to split the incoming request from the
%% Haskell game server into the three components and route the individual
%% components to their respective client connections

%% API for harness
-export([forward/3]).

%% API for match_maker
-export([register_clients/2]).

%% gen_server exports and harness startup
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

%% Test exports
-export([seperate_requests/1]).

-record(state,
        { client_map = maps:new() :: client_map()
        }).

-type client_map() :: maps:maps(common:game_id(), clients()).
-type clients() :: [common:system() | [common:client()]].

-define(NAME, ?MODULE).

-spec forward(common:game_id(), common:cmd(), common:request()) -> ok.
forward(GameID, Cmd, Request) ->
    gen_server:cast(?NAME, {forward, GameID, Cmd, Request}).

-spec register_clients(common:game_id(), clients()) -> ok.
register_clients(GameID, Clients) ->
    gen_server:call(?NAME, {register_clients, GameID, Clients}).

%% Startup
-spec start() -> gen_server:start_ret().
start() ->
    gen_server:start({local, ?NAME}, ?MODULE, [], []).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

%% gen_server exports
init([]) ->
    {ok, #state{}}.

handle_call({register_clients, GameID, Clients}, _From, State) ->
    ClientMap = maps:put(GameID, Clients, State#state.client_map),
    {reply, ok, State#state{client_map = ClientMap}};
%% Call catch-all
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast({forward, GameID, Cmd, Request}, State) ->
    ok = do_forward(GameID, Cmd, Request, State#state.client_map),
    {noreply, State};
%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.

-spec do_forward(common:game_id(), common:cmd(),
                 common:request(), client_map()) -> ok.
do_forward(GameID, Cmd, Request, ClientMap) ->
    [SysReq | Requests] = seperate_requests(Request),
    {ok, [SysPid | Clients]} = maps:find(GameID, ClientMap),

    Fun = fun({Pid, Req}) ->
              ok = client_srvr:forward(Pid, Cmd, Req)
          end,
    lists:foreach(Fun, lists:zip(Clients, Requests)),
    game_system:forward(SysPid, Cmd, SysReq).

%% TODO: add tests
-spec seperate_requests(common:request()) -> [common:request()].
seperate_requests(Request) ->
    seperate_requests(Request, []).

seperate_requests(<<>>, Acc) ->
    lists:reverse(Acc);
seperate_requests(<<Len, Bin/binary>>, Acc) ->
    <<Request:Len/binary, Rest/binary>> = Bin,
    seperate_requests(Rest, [Request | Acc]).
