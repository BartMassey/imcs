#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""A Client for the Internet Minichess Server

This client implements all the relevant commands of the IMCS. Most commands
are used by calling a method with the same name as the IMCS command; however,
a few (like login instead of `me` change_password instead of `password`) are
not.

This module uses the python __enter__ and __exit__ dunder methods to allow the
user to make use of the python with-statement: https://docs.python.org/2.7/reference/
compound_stmts.html#with. This means that all the user of this library must do
to get a logged in player is something like this:

    with Client(username='KnightTime', password='hunter2') as imcs_client:
        games = imcs_client.list()
        # etc...

Whenever this module returns a value, it returns a regular python string as the
IMCS server sends it. This means that any multi-line string will use the unix
CRLF ('\r\n') to end a line."""

from __future__ import print_function

import socket
import sys

__author__ = "Michael Lane"
__email__ = "mikelane@gmail.com"
__copyright__ = "Copyright 2017, Michael Lane"
__license__ = "MIT"

if sys.version_info[:2] < (3, 3):
    python_version = 2
else:
    python_version = 3


class Client:
    """
    A client object for the IMCS server.
    """

    def __init__(self, host='imcs.svcs.cs.pdx.edu', port=3589, username=None, password=None):
        """
        Constructor of the IMCS client.

        Parameters
        ----------
        host : str
            The IMCS server url. Default 'imcs.svcs.cs.pdx.edu'
        port : int
            The IMCS port. Default 3589
        username :
            The username to use to log into the IMCS Server
        password :
            The password to use to log into the IMCS Server
        """
        self.imcs_socket = socket.socket()
        try:
            self.imcs_stream = self.imcs_socket.makefile(mode='rw')
        except TypeError:
            self.imcs_stream = self.imcs_socket.makefile(mode='rw', bufsize=0)

        self.host = host
        self.port = port
        self.username = username
        self.password = password

    def __enter__(self):
        """
        This makes it so the user can do something like this:

            with Server() as s:
                s.get_help()

        The __enter__() function will set up the connection and capture and output the preamble
        lines. It then returns a reference to itself to the caller. The __exit__() function does
        the cleanup operations.

        Returns
        -------
        Client
            The instance to this class.
        """
        print('Connecting to {}:{}'.format(self.host, self.port))
        self.imcs_socket.connect((self.host, self.port))
        print(self.imcs_stream.readline().strip())
        if self.username and self.password:
            print(self.login(self.username, self.password))
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """
        This is run whenever the with block ends. This makes it simple to do cleanup
        operations. For this server, that means sending the quit signal and closing the stream.

        Parameters
        ----------
        exc_type: Exception
            Exception type
        exc_val: str
            Exception value
        exc_tb: str
            Exception traceback

        Returns
        -------
        None
        """
        if exc_type:
            print(exc_type, exc_val)
        self._send('quit')
        print('quit')
        print(self.imcs_stream.readline())
        self.imcs_stream.close()
        self.imcs_socket.close()

    #############
    # UTILITIES #
    #############

    def _send(self, command):
        """
        Send a message to the IMCS server using the imcs_stream file descriptor

        Parameters
        ----------
        command: str
            The string command to _send

        Returns
        -------
        None
        """
        try:
            if python_version == 3:
                print(command, file=self.imcs_stream, flush=True)
            else:
                print(command, file=self.imcs_stream)
                self.imcs_stream.flush()
        except IOError:
            pass

    def _read_lines(self):
        """
        Read a known multiline response from the IMCS server

        Returns
        -------
        str
            Each line read up to and including the ending '.'
        """
        response = self.imcs_stream.readline()
        while '\r\n.\r\n' not in response:
            response += self.imcs_stream.readline()
        return response

    def _read_initial_game_state(self):
        """
        Utility to read the initial game state presented to W on the first move.
        The main difference being that there is no previous opponent's move as
        the first line.

        Returns
        -------
        str
            A string that is the board state including the move number, player
            color, board, and timing information.
        """
        raw_initial_game_state_string = ''
        for _ in range(10):
            raw_initial_game_state_string += self.imcs_stream.readline()
        return raw_initial_game_state_string.strip()

    def read_game_state(self):
        """
        Read and return any game state after the initial game state. If a win,
        loss, or draw has been found, return only the relevant line.

        Returns
        -------
        str
            The game state or win, loss, draw information line as a string that
            is formatted just as the IMCS server sent it.
        """
        raw_game_state_string = self.imcs_stream.readline()
        if '=' in raw_game_state_string:
            return raw_game_state_string
        for _ in range(10):
            raw_game_state_string += self.imcs_stream.readline()
        return raw_game_state_string.strip()

    ###################
    # SIMPLE COMMANDS #
    ###################

    def help(self):
        """
        help: the IMCS help information

        Returns
        -------
        str
            The string of help information that is sent from the IMCS server.
        """
        self._send('help')
        print('help')
        return self._read_lines().strip()

    def list(self):
        """
        list: list available games

        Returns
        -------
        str
            The list of games string sent by the IMCS server.
        """
        self._send('list')
        print('list')
        return self._read_lines().strip()

    def ratings(self):
        """
        ratings: list player ratings (top 10 plus own)

        Returns
        -------
        str
            Player ratings as returned by the IMCS server.
        """
        self._send('ratings')
        print('ratings')
        return self._read_lines().strip()

    def clean(self):
        """
        clean: cancel all my outstanding offers

        Returns
        -------
        str
            The status line returned by the clean command.
        """
        self._send('clean')
        print('clean')
        return self.imcs_stream.readline().strip()

    def rerate(self):
        """
        rerate: reset my rating to the starting default

        Returns
        -------
        str
            The status line returned by the rerate command.
        """
        self._send('rerate')
        print('rerate')
        return self.imcs_stream.readline().strip()

    #################################
    # PARAMETERIZED SIMPLE COMMANDS #
    #################################

    def login(self, username, password):
        """
        Login to the IMCS server with a given username and password.

        me <player> <password>: log in

        Parameters
        ----------
        username : str
            The username to login with
        password : str
            The password to login with

        Returns
        -------
        str
            The IMCS response to the login attempt.
        """
        self.username = username
        self.password = password
        self._send('me {u} {p}'.format(u=username, p=password))
        print('me {u} {p}'.format(u=username, p=password))
        return self.imcs_stream.readline().strip()

    def register(self, username=None, password=None):
        """
        register <player> <password>: register a new name and log in

        Parameters
        ----------
        username : str
            The player name to register
        password : str
            The player's password

        Returns
        -------
        str
            The IMCS response
        """
        self._send('register {u} {p}'.format(u=username, p=password))
        print('register {u} {p}'.format(u=username, p=password))
        return self.imcs_stream.readline().strip()

    def change_password(self, new_password=None):
        """
        password <password>: change password

        Parameters
        ----------
        new_password : str
            The new password string to use.

        Returns
        -------
        str
            The IMCS response
        """
        self._send('password {p}'.format(p=new_password))
        print('password {p}'.format(p=new_password))
        return self.imcs_stream.readline().strip()

    #################################
    # GAME PLAY COMMANDS (BLOCKING) #
    #################################

    def offer(self, my_color='?', my_time=None, opp_time=None):
        """
        Offer a game on the IMCS server

        Parameters
        ----------
        my_color : str
            The color you'd like to use for your player.
        my_time : str or int
            The number of seconds (total) you'd like your player to have.
            (Both players will use this time if opp_time is omitted)
        opp_time : str or int
            The number of seconds (total) you'd like your opponent to have.

        Returns
        -------
        str
            Either the initial game state (if playing white, since that should
            be returned immediately), or the IMCS response if playing as
            black or if there was an error. Note, if playing as black, you will
            need to specifically issue a call to get the game state after this
            returns.
        """
        command = 'offer {c}'.format(c=my_color)
        if my_time:
            command += ' {m}'.format(m=my_time)
        if opp_time:
            command += ' {o}'.format(o=opp_time)
        self._send(command=command)
        print(command)

        response = self.imcs_stream.readline().strip()
        print(response)
        if '103' not in response:
            return response

        response = self.imcs_stream.readline().strip()
        print(response)

        if '105' in response:  # Playing as white
            return self._read_initial_game_state()
        elif '106' in response:  # Playing as black
            return self.read_game_state()
        else:  # Some other error
            return response

    def accept(self, game_number=None, color=None):
        """
        Accept an offered game on the IMCS Server.

        Parameters
        ----------
        game_number : str or int
            The game number to accept. Required.
        color : str
            The color to play as in the accepted game. Optional.

        Returns
        -------
        str
            Either the initial game state (if playing white, since that should
            be returned immediately), or the IMCS response if playing as
            black or if there was an error. Note, if playing as black, you will
            need to specifically issue a call to get the game state after this
            returns.
        """
        command = 'accept {n}'.format(n=game_number)
        if color:
            command += ' {c}'.format(c=color)
        self._send(command=command)
        print(command)

        response = self.imcs_stream.readline().strip()

        if '105' in response:  # Playing as white
            return self._read_initial_game_state()
        elif '106' in response:  # Playing as black
            return self.read_game_state()
        else:  # Some other error
            return response

    def make_move(self, move):
        """
        Send a move to the IMCS. The move should be a string in the form of
        d2-d3. This method will block waiting for the resulting game state
        from the opponent's move.

        Parameters
        ----------
        move : str
            The move string required by the IMCS. For example d2-d3.

        Returns
        -------
        str
            The string of the updated game state after the opponent's move. If,
            for example you were white and your opening move was the queen's
            pawn, you might get this as a response (string quotes omitted):

            ! d5-d4

            kqbnr
            ppp.p
            ...p.
            ...P.
            PPP.P
            RNBQK

            ? 04:59.837 04:58.992

            Where the '! d5-d4' is the move that the opponent made. You might
            also see '- illegal move d4-d2' and the same board you sent out.
        """
        self._send(move)
        return self.read_game_state()

    def resign(self):
        """
        Resigns from a game

        Returns
        -------
        None
        """
        self._send('resign')
        print('resign')
        print(self.imcs_stream.readline().strip())
        print(self.imcs_stream.readline().strip())
