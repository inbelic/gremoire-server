-module(server_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
    start(undefined, undefined).

start(_Type, _Args) ->
    io:format("Hello from server!~n", []),
    ok.

stop(_State) ->
    ok.
