-module(relayer).

%% server to collect responses from the clients of a game and then combine
%% and format them before sending them to the server

%% client_srvr API
-export([respond/2]).

%% match_maker API
-export([register_order/2]).

%% gen_server exports and harness startup
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state,
        { relays = maps:new() :: relays()
        , orders = maps:new() :: orders()
        }).

-type relays() :: maps:maps(common:game_id(), relay()).
-type relay() :: [{common:system() | common:client(), common:response()}].

-type orders() :: maps:maps(common:game_id(), order()).
-type order() :: [common:system() | [common:client()]].

-define(NAME, ?MODULE).

%% API
-spec respond(common:game_id(), ready | common:response()) -> ok.
respond(GameID, Response) ->
    gen_server:cast(?NAME, {respond, GameID, self(), Response}).

-spec register_order(common:game_id(), order()) -> ok.
register_order(GameID, Order) ->
    gen_server:call(?NAME, {register_order, GameID, Order}).

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

handle_call({register_order, GameID, Order}, _From, State) ->
    Orders = maps:put(GameID, Order, State#state.orders),
    Relays = maps:put(GameID, [], State#state.relays),
    {reply, ok, State#state{ relays = Relays, orders = Orders}};
%% Call catch-all
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast({respond, GameID, Pid, Response}, State) ->
    {ok, Relays} = do_respond(GameID, Pid, Response,
                              State#state.relays, State#state.orders),
    {noreply, State#state{relays = Relays}};
%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.


-spec do_respond(common:game_id(), common:client() | common:system(),
                 ready | common:response(),
                 relays(), orders()) -> {ok, relays()}.
do_respond(GameID, Pid, ready, Relays, Orders) ->
    do_respond(GameID, Pid, <<>>, Relays, Orders);
do_respond(GameID, Pid, Response, Relays, Orders) ->
    {ok, Responses0} = maps:find(GameID, Relays),
    {ok, Order} = maps:find(GameID, Orders),
    Responses = [{Pid, Response} | Responses0],
    case length(Responses) == length(Order) of
        false ->
            %% Haven't collected all the required responses to relay to harness
            %% so just put the updated responses back into our Relay map
            {ok, maps:put(GameID, Responses, Relays)};
        true ->
            %% Otherwise, we can respond by first sorting the responses into
            %% the required order and then passing these into do_relay to be
            %% combined and sent accordingly
            OrderedResponses = order_responses(Order, Responses),
            ok = do_relay(GameID, OrderedResponses),
            %% Then we empty the list of responses for the next collection
            {ok, maps:put(GameID, [], Relays)}
    end.


%% For effciency the list returned is the reverse order so that when we
%% iterate with do_relay, it will return it to the correct order without
%% needing to reverse it twice
-spec order_responses(order(), relay()) -> [common:response()].
order_responses(Order, Responses) ->
    order_responses(Order, Responses, []).

order_responses([], [], Acc) ->
    Acc;
order_responses([NextPid | RestOrder], Responses, Acc) ->
    {value, {NextPid, NextResponse}, RestResponses}
        = lists:keytake(NextPid, 1, Responses),
    order_responses(RestOrder, RestResponses, [NextResponse | Acc]).

-spec do_relay(common:game_id(), [common:responses()]) -> ok.
do_relay(GameID, Responses) ->
    %% Fold over the responses and stamp their size for parsing on the other
    %% side, and glue them together in the reverse order as specified above
    Fun = fun(Response, Acc) ->
              Len = byte_size(Response),
              [<<Len, Response/binary>> | Acc]
          end,
    Response = lists:foldl(Fun, [], Responses),
    harness:respond(GameID, Response).
