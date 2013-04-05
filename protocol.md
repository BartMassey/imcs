IMCS Protocol/Use
=================

Connect to the server

    telnet imcs.svcs.cs.pdx.edu 3589

Register a user

    register myusername mypassword

Note that the password will be sent in the clear


Login

    me myusername mypassword

List games

    211 1 available games
    6772 nibz ? 5:00 5:00 1064 [offer]
    .

Offer a game

    offer W 600 600

This will offer a game where you are white, you and your opponent
each have 600 seconds.

Accept a game

    accept 6772

Play a game

    accept 6773
    106 B 8:20 8:20 game starts
    ! a2-a3

Move a piece

    b2-b3


