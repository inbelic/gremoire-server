-module(server_sup).

-behaviour(supervisor).

%% sup callbacks
-export([start_link/3, init/1]).

%% TODO: Add tree structure dipiction here

start_link(ClientPort, HarnessPort, ExtProg) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE,
                          [ClientPort, HarnessPort, ExtProg]).

init([ClientPort, HarnessPort, ExtProg]) ->
    SupFlags
        = #{ strategy => rest_for_one
           },
    ExternalSupSpec
        = #{ id => external_sup
           , start => {external_sup, start_link, [HarnessPort, ExtProg]}
           , modules => [external_sup]
           , type => supervisor
           },
    ClientSupSpec
        = #{ id => client_sup
           , start => {client_sup, start_link, [ClientPort]}
           , modules => [client_sup]
           , type => supervisor
           },
    InfraSupSpec
        = #{ id => infra_sup
           , start => {infra_sup, start_link, []}
           , modules => [infra_sup]
           , type => supervisor
           },
    {ok, {SupFlags, [ExternalSupSpec, ClientSupSpec, InfraSupSpec]}}.
