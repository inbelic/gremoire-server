-module(client_sm).

%% This finite state machine is used to model the state of the
%% interaction between the client and the client_srvr and is
%% used to validate if the client input is valid.

-behaviour(gen_statem).

-include("../include/cmds.hrl").

%% API
-export([update_cmd/2, validate_cmd/3]).

%% gen_statem exports and startup
-export([start/0, start_link/0]).
-export([callback_mode/0, init/1]).

%% custom state_function gen_fsm callbacks
-export([in_lobby/3, in_game/3]).

-type csm() :: pid(). %% csm = client state machine

-export_type([csm/0]).

%% API

%% Update cmd will let us update our game_state data to denote if we are
%% expecting the next message to come from the user or the server
%% (cmd() would expect from client and waiting would expect from server)
-spec update_cmd(csm(), common:cmd() | waiting) -> ok | error.
update_cmd(Csm, Cmd) ->
    gen_statem:call(Csm, {update, Cmd}).

%% We will validate that the command and its response follows the correct
%% format
-spec validate_cmd(csm(), common:cmd(), common:response())
                  -> {valid, common:cmd_group()} | invalid.
validate_cmd(Csm, Cmd, Bin) ->
    gen_statem:call(Csm, {validate, Cmd, Bin}).

%% Startup
-spec start() -> gen_statem:start_ret().
start() ->
    gen_statem:start(?MODULE, [], []).

-spec start_link() -> gen_statem:start_ret().
start_link() ->
    gen_statem:start_link(?MODULE, [], []).

callback_mode() ->
    state_functions.

init([]) ->
    State = in_lobby,
    Data = {},
    {ok, State, Data}.

%% Custom state functions:


%% Lobby validate
in_lobby({call, From}, {validate, Cmd, _}, _Data) when ?IS_GENERAL_CMD(Cmd) ->
    {keep_state_and_data, [{reply, From, {valid, general}}]};
in_lobby({call, From}, {update, ?STARTED}, _Data) ->
    {next_state, in_game, waiting, [{reply, From, ok}]};
in_lobby({call, From}, {validate, Cmd, Bin}, _Data) when ?IS_LOBBY_CMD(Cmd) ->
    Validity = case is_valid_lobby_cmd(Cmd, Bin) of
                 true -> {valid, lobby};
                 false -> invalid
               end,
    {keep_state_and_data, [{reply, From, Validity}]};
%% Lobby catch-all
in_lobby({call, From}, Request, Data) ->
    {keep_state_and_data, [{reply, From, error}]}.

%% Game validate
in_game({call, From}, {validate, Cmd, _}, _Data) when ?IS_GENERAL_CMD(Cmd) ->
    {keep_state_and_data, [{reply, From, {valid, general}}]};
in_game({call, From}, {validate, Cmd, Bin}, _Data) when ?IS_GAME_CMD(Cmd) ->
    Validity = case is_valid_game_cmd(Cmd, Bin) of
                 true -> {valid, game};
                 false -> invalid
               end,
    {keep_state_and_data, [{reply, From, Validity}]};

%% Game update
in_game({call, From}, {update, Cmd}, waiting) when ?IS_GAME_CMD(Cmd) ->
    {keep_state, Cmd, [{reply, From, ok}]};
in_game({call, From}, {update, Cmd}, Cmd) when ?IS_GAME_CMD(Cmd) ->
    {keep_state, waiting, [{reply, From, ok}]};

%% Game catch-all
in_game({call, From}, Request, Data) ->
    {keep_state_and_data, [{reply, From, error}]}.


%% We let ourselves the extensibility to determine what we deem as valid input.
%% For example, checking for profanity in a username or whatever. Hence we
%% will set up the infastructure for such
-spec is_valid_lobby_cmd(common:cmd(), common:response()) -> boolean().
is_valid_lobby_cmd(_Cmd, _Response) ->
    true.

%% We could also check for the formatting of responses. Similar infastructure
%% purposes as above
-spec is_valid_game_cmd(common:cmd(), common:response()) -> boolean().
is_valid_game_cmd(_Cmd, _Response) ->
    true.
