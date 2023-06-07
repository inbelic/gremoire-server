-module(external_sup).

%% Supervisor of the two components that monitor the external Haskell program
%% harness and porter

%% sup callbacks
-export([start_link/2, init/1]).

start_link(HarnessPort, ExtProg) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [HarnessPort, ExtProg]).

init([HarnessPort, ExtProg]) ->
    SupFlags
        = #{ strategy => one_for_all 
           },
    HarnessSpec
        = #{ id => harness
           , start => {harness, start_link, [HarnessPort]}
           , modules => [harness]
           },
    PortSpec
        = #{ id => porter
           , start => {porter, start_link, [HarnessPort, ExtProg]}
           , modules => [porter]
           },
    {ok, {SupFlags, [HarnessSpec, PortSpec]}}.
