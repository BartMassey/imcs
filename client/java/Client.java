// Copyright (c) 2012 Bart Massey <bart@cs.pdx.edu>
// Licensed under the "MIT License"
// Please see the file COPYING at http://github.com/BartMassey/imcs

import java.io.*;
import java.net.*;

/**
 * Provides an interface to the Internet MiniChess Server.
 * <p>
 * See the <a
 * href="http://wiki.cs.pdx.edu/minichess">MiniChess
 * Page</a> for information.
 * The basic workflow is to create a client object connected
 * to the server, use the offer() or accept() method to
 * start a game on that client, and then use the getMove()
 * and sendMove() methods to play the game.
 * <p>
 * A lower-level "expect/send" interface is also provided
 * for those who want to extend the functionality of this
 * class.
 */
public class Client {
    BufferedReader in;
    PrintStream out;
    String sendLineEnding = "\r\n";

    /**
     * Get a response from the server. Blocks until response
     * is received.
     * @param verbose  if true, print the command sent on stdout
     * @return  response
     */
    public String expectResponse(boolean verbose) 
      throws IOException {
	String response;
	while(true) {
	    response = in.readLine();
	    if (response == null)
		throw new IOException("expectResponse: EOF");
	    if (verbose)
		System.out.println(response);
	    if (response.length() < 3)
		continue;
	    int i = 0;
	    for (; i < 3; i++)
		if (!Character.isDigit(response.charAt(i)))
		    break;
	    if (i < 3)
		continue;
	    return response;
	}
    }

    /**
     * Get the response code from a response.
     * @param response  response
     * @return  response code number string
     */
    public static String responseCode(String response) {
	return response.substring(0, 3);
    }

 
    /**
     * Get the response text from a response.
     * @param response  response
     * @return  response string
     */
    public static String responseString(String response) {
	return response.substring(4);
    }

    /**
     * Get a response from the server. Blocks until response
     * is received. Fails if the response code received
     * does not match the response code accepted.
     * @param code  expected code
     * @param verbose  if true, print the command sent on stdout
     * @return  response
     */
    public String expect(String code, boolean verbose) 
      throws IOException {
	String response = expectResponse(verbose);
	return responseString(response);
    }

    /**
     * Send a command to the server.
     * @param cmd  command to send
     * @param verbose  if true, print the command sent on stdout
     */
    public void send(String cmd, boolean verbose) {
	if (verbose)
	    System.out.println(cmd);
	out.print(cmd + sendLineEnding);
	out.flush();
    }

    /**
     * Create a new client connected to IMCS and logged on.
     * @param server  hostname or IP address of IMCS server, usually "imcs.svcs.cs.pdx.edu"
     * @param portStr  server port number string, usually "3589"
     * @param username  username of account on IMCS server
     * @param password  password of account on IMCS server
     */
    public Client(String server, String portStr,
	          String username, String password) throws IOException {
	sendLineEnding = "\r\n";
	setClient(server, portStr, username, password);
    }
    
    /**
     * Create a new client connected to IMCS and logged on.
     * @param server  hostname or IP address of IMCS server, usually "imcs.svcs.cs.pdx.edu"
     * @param portStr  server port number string, usually "3589"
     * @param username  username of account on IMCS server
     * @param password  password of account on IMCS server
     * @param sendLineEnding  what to use for line ending when sending
     */
    public Client(String server, String portStr,
	          String username, String password,
		  String sendLineEnding) throws IOException {
	this.sendLineEnding = sendLineEnding;
	setClient(server, portStr, username, password);
    }
    
    void setClient(String server, String portStr,
	           String username, String password) throws IOException {
	int port = Integer.parseInt(portStr);
	Socket s = new Socket(server, port);
	InputStreamReader isr =
	    new InputStreamReader(s.getInputStream());
	in = new BufferedReader(isr);
	out = new PrintStream(s.getOutputStream(), true);
	String version = expectResponse(false);
	if (!"imcs 2.5".equals(responseString(version)))
	    throw new Error("client: imcs version mismatch");
	send("me " + username + " " + password, true);
	expect("201", true);
    }

    /**
     * Get a move string from the IMCS server. Blocks
     * until move is received.
     * @return  opponent move string
     */
    public String getMove()
      throws IOException {
	String line;
	char ch;
	while (true) {
	    line = in.readLine();
	    if (line == null)
		return null;
	    System.out.println(line);
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
    public void sendMove(String moveStr) 
      throws IOException {
	String line;
	do {
	    line = in.readLine();
	    if (line == null)
		throw new IOException("server terminated unexpectedly");
	    System.out.println(line);
	} while (line.length() == 0 || line.charAt(0) != '?');
	System.out.println(moveStr);
	out.print(moveStr + sendLineEnding);
	out.flush();
    }

    /**
     * Instruct the server to offer a game to
     * other players. If color is "?", allow the other side
     * or the server to pick the color.
     * Blocks until the offer is accepted.
     * @param color  the color your side wants to play
     * @return  the color that your side should play
     */
    public char offer(char color) 
      throws IOException {
	if (color == '?')
	    send("offer", true);
	else
	    send("offer " + color, true);
	expect("103", true);
	String code = responseCode(expectResponse(true));
	if (code.equals("105"))
	    return 'W';
	else if (code.equals("106"))
	    return 'B';
	throw new IOException("offer: unknown response code");
    }

    /**
     * Instruct the server to accept a game.
     * If color is "?", allow the other side
     * or the server to pick the color.
     * @param id  game id number string
     * @param color  the color that your side wants to play
     * @return  the color that your side should play
     */
    public char accept(String id, char color)
      throws IOException {
	if (color == '?')
	    send("accept " + id, true);
	else
	    send("accept " + id + " " + color, true);
	String code = responseCode(expectResponse(true));
	if (code.equals("105"))
	    return 'W';
	if (code.equals("106"))
	    return 'B';
	throw new IOException("accept: unknown response code");
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
}
