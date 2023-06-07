%% Module adapted from learn you erlang
-module(client_sup).

%% Supervisor module to allow for client connections and spawn a client_srvr
%% for each of these

-behaviour(supervisor).

%% Behaviour callbacks
-export([start_link/1, init/1]).

%% Start a dynamic child
-export([start_socket/0]).

-define(NAME, ?MODULE).

start_link(Port) ->
    supervisor:start_link({local, ?NAME}, ?MODULE, [Port]).

init([Port]) ->
    %% See client_srvr
    {ok, ListenSock} = gen_tcp:listen(Port,
                                        [ binary
                                        , {active, once}
                                        , {packet, 1}
                                        , {reuseaddr, true}
                                        ]),
    spawn_link(fun empty_listeners/0),
    SupFlags
        = #{ strategy => simple_one_for_one
           , intensity => 60
           , period => 3600
           },
    ClientSpec
        = #{ id => client_srvr
           , start => {client_srvr, start_link, [[ListenSock]]}
           , restart => temporary   %% Connection dropped no need to restart
           , modules => [client_srvr]
           },
    {ok, {SupFlags, [ClientSpec]}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,20)],
    ok.
