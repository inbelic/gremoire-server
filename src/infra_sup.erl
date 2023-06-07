-module(infra_sup).

%% The supervisor over the various internal 'infastructure components' around
%% relaying and routing messages to the correct clients of a particular game

-behaviour(supervisor).

%% sup callbacks
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags
        = #{ strategy => one_for_one
           },
    RouterSpec
        = #{ id => client_router
           , start => {client_router, start_link, []}
           , modules => [client_router]
           },
    RelayerSpec
        = #{ id => relayer
           , start => {relayer, start_link, []}
           , modules => [relayer]
           },
    MatchMakerSpec
        = #{ id => match_maker
           , start => {match_maker, start_link, []}
           , modules => [match_maker]
           },
    {ok, {SupFlags, [RouterSpec, RelayerSpec, MatchMakerSpec]}}.
