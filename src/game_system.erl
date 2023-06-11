-module(game_system).

%% This module implements the logic of a server to provide random targets
%% and to order system generated targets automatically

-behaviour(gen_server).
-include("../include/cmds.hrl").

%% API for client_router.erl
-export([forward/3]).

%% gen_server exports and harness startup
-export([start/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state,
        { game_id  :: common:game_id()
        , rand     :: rand:state()
        }).

%% API
-spec forward(common:system(), common:cmd(), common:request()) -> ok.
forward(Pid, Cmd, Req) ->
    gen_server:cast(Pid, {forward, Cmd, Req}).

%% Startup
-spec start(common:game_id()) -> gen_server:start_ret().
start(GameID) ->
    gen_server:start(?MODULE, [GameID], []).

-spec start_link(misc:game_id()) -> gen_server:start_ret().
start_link(GameID) ->
    gen_server:start_link(?MODULE, [GameID], []).

init([GameID]) ->
    Seed = rand:seed(exsss),
    {ok, #state{game_id = GameID, rand = Seed}}.

%% Call catch-all
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast({forward, Cmd, Req}, State) ->
    {ok, State1} = do_handle_forward(Cmd, Req, State),
    {noreply, State1};
%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.


%% We only need to consider the Game commands
-spec do_handle_forward(common:cmd(), common:request(), State :: term()) ->
                        {ok, State :: term()}.
do_handle_forward(_, <<>>, #state{game_id = GameID} = State) ->
    %% No action to take
    ok = relayer:respond(GameID, ready),
    {ok, State};
do_handle_forward(?DISPLAY, _, #state{game_id = GameID} = State) ->
    %% Not concerned about what is displayed
    ok = relayer:respond(GameID, ready),
    {ok, State};
do_handle_forward(?ORDER, Req, #state{game_id = GameID} = State) ->
    %% Orders are packed as 2 bytes segments of CardID and AbilityID so we
    %% take the size of the Req and divide it by 2 to get the total number of
    %% triggers we need to order
    Len = byte_size(Req) div 2,
    Order = lists:seq(1, Len),
    ok = relayer:respond(GameID, list_to_binary(Order)),
    {ok, State};
do_handle_forward(?TARGET, Req, #state{game_id = GameID} = State) ->
    <<_CardID, _AbilityID, Targets/binary>> = Req,
    %% Targets are then packed as 1 byte segments of the CardID of potential
    %% targets so we take the size of the Req and generate a random index of the
    %% bytes, then we return that randomly selected byte
    Len = byte_size(Targets),
    {TargetIdx, RandState} = get_rand_idx(Len, State#state.rand),
    Target = binary:at(Targets, TargetIdx),
    ok = relayer:respond(GameID, <<Target>>),
    {ok, State#state{rand = RandState}};
do_handle_forward(?RESULT, _, State) ->
    %% FIXME: need to do something to let the relay/router cleanup
    {stop, normal, State}.

-spec get_rand_idx(integer(), rand:state()) -> {integer(), rand:state()}.
-ifdef(debug).
%% When testing we want the random behaviour to be in a fixed order
get_rand_idx(_Len, RandState) ->
  {0, RandState}.
-else.
get_rand_idx(Len, RandState0) ->
    {RandIdx, RandState} = rand:uniform_s(Len, RandState0),
    TargetIdx = RandIdx - 1, %% Account for 0 indexing of bytes
    {TargetIdx, RandState}.
-endif.
