# Internet MiniChess Server

Copyright &copy; 2007-2017 Bart Massey  
Please see the file `COPYING` in this distribution for license
information.

This package is a communications server and referee for
games of MiniChess played online. See
<http://wiki.cs.pdx.edu/minichess/> for more information on
MiniChess.

Building this package requires a Haskell compiler; it is
known to work with GHC 8.0.1. This package requires the
following extra packages available from Hackage:

  * parseargs: provides `System.Console.ParseArgs`, my argument parser
  * regex-tdfa: provides `Text.Regex.TDFA`
  * network: provides `Network`
  * random: provides `System.Random` (may be provided in newer libs)
  * unbounded-delays: provides `Control.Concurrent.Timeout`

All are also available as Debian packages.

The program provides a service to which a client can connect
and either offer a game or accept a game from an existing
client.  Once both players are in place, the service accepts
moves from each side in turn on its connection.  The server
prompts each side with the game state to allow easy sanity
checks. It also acts as a referee, preventing illegal moves
and determining game outcome.

## Invocation

There should be a subdirectory called `imcsd/` of the
invocation directory in which imcs can write game state
and logs.  You can get this set up by running imcs with
the `--init` flag.  This is also how you upgrade to new
versions of imcs.  You may run `--init` to initialize or
upgrade, or just as a convenient way to stop the server.
From version 2.2 on, `--init` takes an argument which is the
password for the user "admin", who should be registered
with imcs.  This password is used by the new "stop"
command, which will put the server in a stopping state,
wait until all current games have been played, and then
cause the server to exit.

The `--port <n>` argument sets the server listening port;
the current default port is, for no very good reason, 3589.
