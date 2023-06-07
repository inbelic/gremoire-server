-module(porter).

%% Port to the external Haskell program to allow a strangling process to monitor
%% it. It is responsible for triggering restart or shutdown according to our
%% erlang supervisor tree

-behaviour(gen_server).

%% gen_server exports and port startup
-export([start/2, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state,
        { port
        , handle
        }).

%% Startup
-spec start(integer(), string()) -> gen_server:start_ret().
start(HarnessPort, ExtProg) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [HarnessPort, ExtProg], []).

-spec start_link(integer(), string()) -> gen_server:start_ret().
start_link(HarnessPort, ExtProg) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [HarnessPort, ExtProg], []).

%% gen_server exports
init([HarnessPort, ExtProg]) ->
    {ok, Handle} = file:open("port.logs", [write]),
    {ok, Port} = start_port(HarnessPort, ExtProg),
    {ok, #state{port = Port, handle = Handle}}.

%% Call catch-all
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

%% Cast catch-all
handle_cast(_Request, State) ->
    {noreply, State}.

%% Capture ExtProg stdout
handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    Handle = State#state.handle,
    io:format(Handle, "~p~n", [Data]),
    {noreply, State};
handle_info({'EXIT', Port, normal}, #state{port = Port} = State) ->
    {stop, normal, State};
handle_info({'EXIT', _From, normal}, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% Terminate catch-all
terminate(_Reason, #state{port = Port, handle = Handle} = _State) ->
    Port ! {self(), close},
    ok = wait_until_close(Handle, Port),
    file:close(Handle),
    ok.

wait_until_close(Handle, Port) ->
    receive
      {Port, closed} ->
          io:format(Handle, "port closed gracefully...~n", []),
          ok;
      {'EXIT', Port, Reason} ->
          io:format(Handle, "closing reason: ~p~n", [Reason]),
          wait_until_close(Handle, Port);
      Info ->
          io:format(Handle, "other info while closing: ~p~n", [Info]),
          wait_until_close(Handle, Port)
    end.

-spec start_port(integer(), string()) -> {ok, pid()}.
start_port(HarnessPort, ExtProg) ->
    %% We can crash out here if we want to so just assume things are right
    %% and the program is in the path or dir
    ExtProgPath = os:find_executable(ExtProg),
    process_flag(trap_exit, true),
    Port = open_port({spawn_executable, ExtProgPath},
                     [ use_stdio, {packet, 1}, in,
                      {args, [integer_to_binary(HarnessPort)]}
                     ]),
    link(Port),
    {ok, Port}.
