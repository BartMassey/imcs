# Client for the Internet Minichess Server

This client implements all the relevant commands of the IMCS. Most commands
are used by calling a method with the same name as the IMCS command; however,
a few (like login instead of `me` change_password instead of `password`) are
not.

That said, the user will not ever need to explicitly call the `login()` method. 
This module uses the python `__enter__` and `__exit__` dunder methods to allow the
user to make use of the python with-statement. See the Usage section below for more
information.

Whenever this module returns a value, it returns a regular python string as the
IMCS server sends it. This means that any multi-line string will use the unix
`CRLF` (`\r\n`) to end a line."

### Installation

You should be able to simply run the following:

    python setup.py build
    python setup.py install

This should install any requirements (the only requirement is `configparser` which
back ports the configparser module to to Python 2). If you desire to run the unit
tests, you'll need to create a `settings.ini` file in the `imcs-client/tests`
folder. See the file `example-settings.ini` for the required format of this file.

### Usage

To use this in your script you should first import the `Client` class.

    from imcs-client.client import Client
    
Then the only thing you need to do to log in is to use a python [`with`-statement](https://docs.python.org/2.7/reference/compound_stmts.html#with).
This means that all the user of this library must do to get a logged in player 
is something like this:

    with Client(username='KnightTime', password='hunter2') as imcs_client:
        games = imcs_client.list()
        # etc...

Furthermore, there is nothing the user must explicitly do to quit the IMCS server
since the `__exit__()` method will be called whenever the `with`-statement goes out
of scope and quitting the IMCS server and closing the socket are implemented in the
`__exit__()` method.