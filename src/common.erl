-module(common).

-include("../include/cmds.hrl").

%% Module used to export various common types

-export_type([game_id/0]).
-export_type([system/0, client/0, cmd/0, cmd_group/0]).
-export_type([request/0, response/0]).

-type game_id()         :: integer().

-type system()          :: pid().
-type client()          :: pid().
-type u8()              :: 0..255.
-type cmd()             :: u8().
-type cmd_group()       :: general | lobby | game.

-type request()         :: binary().
-type response()        :: iolist() | binary().
