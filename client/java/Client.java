// Copyright (c) 2012 Bart Massey <bart@cs.pdx.edu>
// Copyright (c) 2017 Markus Ebner <markus-ebner@web.de>
// Licensed under the "MIT License"
// Please see the file COPYING at http://github.com/BartMassey/imcs

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Provides a quite complete interface to the Internet MiniChess Server.
 * <p>
 * See the <a href="http://wiki.cs.pdx.edu/minichess">MiniChess Page</a> for information.
 * The basic workflow is to create a client object (which conntects to the IMCS).
 * Login with your credentials, and then either join a game with the accept() method
 * or use the offerAndWait() method to offer an own game.
 *
 * The methods getMove() and sendMove() are used during a match, to receive the move
 * the opponent did, and send the own move to the opponent, respectively.
 * <p>
 */
public class Client {
    BufferedReader in;
    PrintStream out;

    /**
     * Enum of all commands that the IMCS supports.
     * The fields are exactly called like the commands but uppercase.
     * Thus, the enum.toString().toLowerCase() value is good to be sent over the line.
     */
    enum IMCSCommands {
        HELP,
        QUIT,
        ME,
        REGISTER,
        PASSWORD,
        LIST,
        RATINGS,
        OFFER,
        ACCEPT,
        CLEAN,
        RERATE
    }



    /* ######################## */
    /* # LIFE-TIME MANAGEMENT # */
    /* ######################## */

    /**
     * Create a new client connected to IMCS and logged on.
     * @param server  hostname or IP address of IMCS server, usually "imcs.svcs.cs.pdx.edu"
     * @param port  server port number, usually 3589
     * @throws IOException when the NetworkStream was unexpectedly closed.
     */
    public Client(String server, int port) throws IOException {
        Socket s = new Socket(server, port);
        InputStreamReader isr = new InputStreamReader(s.getInputStream());
        in = new BufferedReader(isr);
        out = new PrintStream(s.getOutputStream(), true);
        IMCSResponse versionResponse = awaitResponse();
        versionResponse.assertHasCode(100);
        if (!versionResponse.message.equals("imcs 2.5"))
            throw new Error("client: imcs version mismatch");
    }

    /**
     * Create a new client connected to IMCS and logged on.
     * @param server  hostname or IP address of IMCS server, usually "imcs.svcs.cs.pdx.edu"
     * @param portStr  server port number string, usually "3589"
     * @throws IOException when the NetworkStream was unexpectedly closed.
     */
    public Client(String server, String portStr) throws IOException {
        this(server, Integer.parseInt(portStr));
    }

    /**
     * Closes the connection to the server. Do not use the
     * object after this.
     */
    public void close()
            throws IOException {
        in.close();
        out.close();
    }







    /* ############################ */
    /* # GENERAL PROTOCOL METHODS # */
    /* ############################ */

    /**
     * Wait for a response on the line.
     * A response has the format:  [000] [Message]
     * @return Response, containing the response line split up in code and message.
     * @throws IOException when the NetworkStream was unexpectedly closed.
     */
    private IMCSResponse awaitResponse() throws IOException {
        IMCSResponse response = null;
        do {
            response = IMCSResponse.parse(in.readLine());
            if(response == null)
                throw new IOException("Broken pipe");
        } while(response == null);
        return response;
    }

    /**
     * Read a complete listing (after the response line) into a String array.
     * @return A list of items in the listing.
     * @throws IOException when the NetworkStream was unexpectedly closed.
     */
    private List<String> awaitListing() throws IOException {
        List<String> result = new ArrayList<>();
        while(true) {
            String line = in.readLine();
            if(line == null)
                throw new IOException("Broken pipe");
            if(line.charAt(0) == '.')
                break;
            result.add(line);
        }
        return result;
    }

    /**
     * Send the given command with the given arguments to the IMCS.
     * @param command Command to send to the IMCS.
     * @param cmdArgs Arguments for the command that should be sent to the IMCS.
     */
    private void sendCommand(IMCSCommands command, Object... cmdArgs) {
        String cmdLine = command.toString().toLowerCase();
        for(Object cmdArg : cmdArgs)
            cmdLine += " " + cmdArg.toString();
        out.println(cmdLine);
        out.flush();
    }





    /* ################### */
    /* # COMMAND METHODS # */
    /* ################### */

    /**
     * Try to login to the IMCS with the given username and password.
     * @param username Username to use for logging into the IMCS.
     * @param password Password to use for logging into the IMCS.
     * @throws IOException when the NetworkStream was unexpectedly closed.
     * @throws RuntimeException When the required response code does not match the one received.
     */
    public void login(String username, String password) throws IOException, RuntimeException {
        sendCommand(IMCSCommands.ME, username, password);
        awaitResponse().assertHasCode(201);
    }

    /**
     * Change the currently authenticated user's password to the given new password.
     * @param newPassword Password to change the currently authenticated user's password into.
     * @throws IOException when the NetworkStream was unexpectedly closed.
     * @throws RuntimeException When the required response code does not match the one received.
     */
    public void changePassword(String newPassword) throws IOException, RuntimeException {
        sendCommand(IMCSCommands.PASSWORD, newPassword);
        awaitResponse().assertHasCode(203);
    }

    /**
     * Request a rerate for the account currently logged in.
     * @throws IOException when the NetworkStream was unexpectedly closed.
     * @throws RuntimeException When the required response code does not match the one received.
     */
    public void rerate() throws IOException, RuntimeException {
        sendCommand(IMCSCommands.RERATE);
        awaitResponse().assertHasCode(206);
    }


    /**
     * Get a list of all games known to the IMCS.
     * @return A list of all games known to the IMCS.
     * @throws IOException when the NetworkStream was unexpectedly closed.
     * @throws RuntimeException When the required response code does not match the one received.
     */
    public List<IMCSGame> getGameList() throws IOException, RuntimeException {
        sendCommand(IMCSCommands.LIST);
        awaitResponse().assertHasCode(211);
        return awaitListing().stream()
                .map(line -> IMCSGame.parse(line))
                .filter(g -> g != null)
                .collect(Collectors.toList());
    }

    public Map<String, Integer> getRatingsList() throws IOException, RuntimeException {
        sendCommand(IMCSCommands.RATINGS);
        awaitResponse().assertHasCode(212);
        return awaitListing().stream()
            .collect(Collectors.toMap(
                l -> l.trim().split(" ", 2)[0],
                l -> Integer.parseInt(l.trim().split(" ", 2)[1])
            ));
    }


    /**
     * Offer a default match where the Server selects the player.
     * @return Player (either W or B) the server chose for this client.
     * @throws IOException when the NetworkStream was unexpectedly closed.
     * @throws RuntimeException When the required response code does not match the one received.
     */
    public char offerGameAndWait() throws IOException, RuntimeException {
        sendCommand(IMCSCommands.OFFER);
        awaitResponse().assertHasCode(103); // waiting for opponent
        IMCSResponse gameStartResponse = awaitResponse();
        gameStartResponse.assertHasCode(106); // game started
        return gameStartResponse.message.charAt(0);
    }

    /**
     * Offer a default match where the Server selects the player.
     * @param player Player to play with in the offered game.
     * @throws IOException when the NetworkStream was unexpectedly closed.
     * @throws RuntimeException When the required response code does not match the one received.
     */
    public void offerGameAndWait(char player) throws IOException, RuntimeException {
        sendCommand(IMCSCommands.OFFER);
        awaitResponse().assertHasCode(103); // waiting for opponent
        awaitResponse().assertHasCode(106); // game started
    }


    /**
     * Accept an offered match with the given gameId, requesting the given player.
     * @param gameId Id of the offered game to join.
     * @param player Player (either B or W) to request to play with in the game (may fail if already taken).
     * @throws IOException when the NetworkStream was unexpectedly closed.
     * @throws RuntimeException When the required response code does not match the one received.
     */
    public void accept(String gameId, char player) throws IOException, RuntimeException {
        sendCommand(IMCSCommands.ACCEPT, gameId, player);
        awaitResponse().assertHasCode(105, 106); // game started
    }

    /**
     * Accept an offered match with the given gameId, letting the server select the players.
     * @param gameId Id of the offered game to join.
     * @return Player (either W or B) the server chose for this client.
     * @throws IOException when the NetworkStream was unexpectedly closed.
     * @throws RuntimeException When the required response code does not match the one received.
     */
    public char accept(String gameId) throws IOException, RuntimeException {
        sendCommand(IMCSCommands.ACCEPT, gameId);
        IMCSResponse gameStartResponse = awaitResponse();
        int playerCode = gameStartResponse.assertHasCode(105, 106); // game started
        return (playerCode == 105) ? 'W' : 'B';
    }





    /* ################ */
    /* # GAME METHODS # */
    /* ################ */

    /**
     * Get a move string from the IMCS server. Blocks
     * until move is received.
     * @return  opponent move string
     */
    public String getMove() throws IOException {
        String line;
        char ch;
        while (true) {
            line = in.readLine();
            if (line == null)
                return null;
            if (line.length() == 0)
                continue;
            ch = line.charAt(0);
            if (ch == '!' || ch == '=')
                break;
        }
        if (ch == '=')
            return null;
        return line.substring(2);
    }

    /**
     * Send a move to the server.
     * @param moveStr  move string to send
     */
    public void sendMove(String moveStr) throws IOException {
        String line;
        do {
            line = in.readLine();
            if (line == null)
                throw new IOException("server terminated unexpectedly");
        } while (line.length() == 0 || line.charAt(0) != '?');
        out.println(moveStr);
        out.flush();
    }

}
