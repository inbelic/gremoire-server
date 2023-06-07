%% Here we define all the commands we expect to receive throughout the system

%% Current clientside cmds
%% Cmd :: enum u8 {
%%     OK, INVALID, ECHO,
%%     LOGIN, QUEUE, STARTED,
%%     DISPLAY, ORDER, TARGET, RESULT,
%% } // Corresponding to the cmds in server/include/cmds.hrl serverside

%% General
-define(IS_GENERAL_CMD(X),
        X == ?OK orelse
        X == ?INVALID orelse
        X == ?ECHO
       ).

-define(OK, 0).
-define(INVALID, 1).
-define(ECHO, 2).

%% Lobby
-define(IS_LOBBY_CMD(X),
        X == ?LOGIN orelse
        X == ?QUEUE orelse
        X == ?STARTED
       ).

-define(LOGIN, 3).
-define(QUEUE, 4).
-define(STARTED, 5).

%% Game
-define(IS_GAME_CMD(X),
        X == ?DISPLAY orelse
        X == ?ORDER orelse
        X == ?TARGET orelse
        X == ?RESULT
       ).

-define(DISPLAY, 6).
-define(ORDER, 7).
-define(TARGET, 8).
-define(RESULT, 9).
