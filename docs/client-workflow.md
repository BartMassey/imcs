# IMCS Client Workflow
Spencer Krumm

Here's an example of client workflow for interacting with IMCS.
Hopefully this is sufficient to get you started.

## Connect to the server

        telnet imcs.svcs.cs.pdx.edu 3589

## Get help

        help

## Register a user

        register myusername mypassword

Note that the password will be sent in the clear. It is
intended to prevent spam and screwups, not as a serious
security measure.

## Login

        me myusername mypassword

## List games

        list

The server responds with a listing of available games.

        211 1 available games
         6772 nibz ? 5:00 5:00 1064 [offer]
        .

## Offer a game

        offer W 600 600

This will offer a game where you are white, you and your opponent
each have 600 seconds. This is the default, so you can just say

        offer W

There is the possibility of offering without caring which color
you get

        offer ?

If the accepting opponent chooses a color, you will get the
opposing color. Otherwise, the server will do a coin flip.

## Accept a game

        accept 6772

If you want to accept as a specific color, you can.

        accept 6772 W

If the offering player already took your color, the
server will refuse the accept.

## Play a game

Once the offer and acceptance are complete, you get game
start info.  Assume that you are playing black. You might
see:

        106 B 5:00 5:00 game starts
        ! a2-a3

        1 B
        kqbnr
        ppppp
        .....
        P....
        .PPPP
        RNBQK

        ? 05:00.000 05:00.000

`a2-a3` is white's move. The `?` indicates that it is your
turn.  The time on the left is always your clock time
remaining (regardless of color). The time on the right is
your opponent's clock.

## Move a piece

To move a piece, you simply send a move string.

        b2-b3

If the move is illegal, the server's referee will tell you
this.

        - illegal move b2-b3

If the move is legal, it is silently accepted. You will
be notified as above when it is your turn again.


## Game over

There are two ways the game can end. First, a legal move is
`resign`, which does the obvious thing. Second, the game
can be over by natural win, loss, or draw.

When the game is over, the server reports the result to both
sides.

        = B wins on resignation
        232 B wins on resignation

The server then closes the connection to both sides. If you
want a new game, you will have to reconnect to the server
and start from the beginning again.

It is polite to wait for a game report from the server
before closing your client connection, even if you "know"
you've lost.
