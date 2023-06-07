-module(match_maker).

%% server to handle match-making of queued clients

-behaviour(gen_server).

-include("../include/cmds.hrl").

%% client_srvr API
-export([queue/2]).

%% gen_server exports and harness startup
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state,
        { queues = maps:new() :: queues()
        , game_ids = []       :: [common:game_id()]
        }).

-record(user_info,
        { name :: string()
        }).

-type queues()     :: maps:maps(common:game_id(), queue()).
-type queue()      :: [{pid(), user_info(), binary()}].
-type user_info()  :: #user_info{}.

-export_type([user_info/0]).

%% APIs
-spec queue(user_info(), binary()) -> ok.
queue(UserInfo, ConfigBin) ->
    gen_server:cast(?MODULE, {queue, self(), UserInfo, ConfigBin}).

%% Startup
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server exports
init([]) ->
    {ok, #state{}}.

%% Call catch-all
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast({queue, Pid, UserInfo, ConfigBin}, State) ->
    State1 = do_queue(Pid, UserInfo, ConfigBin, State),
    {noreply, State1};
%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.

-spec do_queue(pid(), user_info(), binary(), State :: term())
              -> State :: term().
do_queue(Pid, UserInfo, ConfigBin, #state{} = State) ->
    %% Get queue
    {GameID, Queue0} = determine_queue(UserInfo, State),
    Queue = [{Pid, UserInfo, ConfigBin} | Queue0],

    %% Update GameIDs
    GameIDs = case lists:member(GameID, State#state.game_ids) of
                true -> State#state.game_ids;
                false -> [GameID | State#state.game_ids]
              end,

    %% See if we can start a game with the queue
    Queues = case run_queue(GameID, Queue) of
               not_ready ->
                  %% We can just update our Queue to include the queuing client
                  maps:put(GameID, Queue, State#state.queues);
               ready ->
                  %% Otherwise, the queue has started a game and we remove it
                 maps:remove(GameID, State#state.queues)
             end,

    %% Return updated
    State#state{queues = Queues, game_ids = GameIDs}.


%% do_queue Helper functions
-spec determine_queue(user_info(), State :: term())
                      -> {common:game_id(), queue()}.
determine_queue(UserInfo, State) ->
    case find_match(UserInfo, State#state.queues) of
        nomatch ->
            %% Place the client into a new queue and return an emtpy queue
            GameID = next_game_id(State#state.game_ids),
            {GameID, []};
        {match, GameID, Queue} ->
            %% We found a match for the client and so return that queue
            {GameID, Queue}
    end.

%% Find the best match for the client
-spec find_match(user_info(), queues()) -> nomatch
                                         | {match, common:game_id(), queue()}.
find_match(UserInfo, Queues) ->
    %% Define our folding function
    Fun = fun(GameID, Queue, nomatch) ->
                  Val = match_heuristic(UserInfo, Queue),
                  case 0 < Val of
                      true -> {match, {GameID, Queue}, Val};
                      false -> nomatch
                  end;
             (GameID, Queue, {match, _, BestVal} = Match) ->
                  Val = match_heuristic(UserInfo, Queue),
                  case BestVal < Val of
                      false ->
                          Match;
                      true ->
                          {match, {GameID, Queue}, Val}
                  end
          end,

    %% Fold over our map and see if can find a match
    case maps:fold(Fun, nomatch, Queues) of
        nomatch ->
            nomatch;
        {match, {GameID, Queue}, _Val} ->
            {match, GameID, Queue}
    end.

%% Can do whatever heuristic here to find opponents when the game has millions
%% of players to search upon ;) for now we just play against the only other
%% player on the server (most likely myself)
match_heuristic(_UserInfo, _Queue) ->
    1.

%% When ready is returned we have spawned a worker to start a new game. When
%% not_ready is returned then we have not started a new game and the queue must
%% keep waiting
-spec run_queue(common:game_id(), queue()) -> ready | not_ready.
run_queue(_, []) ->
    not_ready; %% Shouldn't need this case
run_queue(_, [_OnePlayer]) ->
    not_ready;
run_queue(GameID, Queue) ->
    Fun = fun() -> start_game(GameID, Queue) end,
    spawn(Fun),
    ready.

-spec next_game_id([common:game_id()]) -> common:game_id().
next_game_id(GIDs) ->
    length(GIDs) + 1.


%% Functions surrounding the start of a new game and the worker that is spawned
%% to do so
-spec start_game(common:game_id(), queue()) -> ok.
start_game(GameID, Queue) ->
    %% We first need to randomize the order of the queue
    RandomQueue = randomize_queue(Queue),
    %% Then we can strip the user info from the Queue and unzip it
    {Clients, Configs} = lists:unzip(
                           lists:map(fun({Pid, _, ConfigBin}) ->
                                         {Pid, ConfigBin}
                                     end, RandomQueue)
                          ),
    %% Allocate a game_system helper for the new game
    {ok, SysPid} = game_system:start(GameID),
    Pids = [SysPid | Clients],

    %% Register our clients to be properly routed
    ok = client_router:register_clients(GameID, Pids),
    ok = relayer:register_order(GameID, Pids),

    %% Notify all the clients that the game has started
    lists:foreach(fun(Pid) ->
                      client_srvr:forward(Pid, ?STARTED, <<GameID:32>>)
                  end, Clients),

    %% Notify the game server to start the game with our configs
    harness:respond(GameID, Configs),
    ok.

-spec randomize_queue([common:game_id()]) -> [common:game_id()].
randomize_queue(List) ->
    randomize_queue(List, length(List), []).

randomize_queue([], 0, Acc) ->
    Acc;
randomize_queue(List, N, Acc) ->
    Index = rand:uniform(N) - 1,
    {Front, [Choosen | Back]} = lists:split(Index, List),
    randomize_queue(Front ++ Back, N - 1, [Choosen | Acc]).
