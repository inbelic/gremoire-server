-module(server_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
    start(normal, [3565, 3568, "gremoire"]).

start(_Type, [ClientPort, HarnessPort, ExtProg]) ->
    server_sup:start_link(ClientPort, HarnessPort, ExtProg).

stop(_State) ->
    ok.
