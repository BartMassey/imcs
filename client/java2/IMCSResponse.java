// Copyright (c) 2017 Markus Ebner <markus-ebner@web.de>
// Licensed under the "MIT License"
// Please see the file COPYING at http://github.com/BartMassey/imcs

import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Class representing one response line from the IMCS.
 * This class does the parsing and offers a convenience method for checking the code.
 */
class IMCSResponse {
    public int code;
    public String message;

    /**
     * Check whether this Response has the given required code. If not, throw a {@see RuntimeException}.
     * @param requiredCode Code to check the Response's code against.
     * @throws RuntimeException When the required response code does not match the one we got.
     */
    public void assertHasCode(int requiredCode) {
        if(code != requiredCode)
            throw new RuntimeException("Got unexpected response: " + code + " " + message);
    }

    /**
     * Try to parse the given string as a response line from the IMCS.
     * @param line String to parse as response line from the IMCS.
     * @return {@code true} when the given String was a valid response line, {@code false} otherwise.
     */
    public static IMCSResponse parse(String line) throws IOException {
        Matcher matcher = responsePattern.matcher(line);
        if(!matcher.matches()) return null;
        IMCSResponse response = new IMCSResponse();
        response.code = Integer.parseInt(matcher.group(1));
        response.message = matcher.group(2);
        return response;
    }
    private static final Pattern responsePattern = Pattern.compile("([0-9]{3}) (.*)");
}
