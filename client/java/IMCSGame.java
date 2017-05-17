// Copyright (c) 2017 Markus Ebner <markus-ebner@web.de>
// Licensed under the "MIT License"
// Please see the file COPYING at http://github.com/BartMassey/imcs

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Game object containing information of an either offered or currently running game on the IMCS.
 */
public class IMCSGame {
    public String gameId;
    public char reservedPlayer = 0;
    public boolean isRunning = false;

    /**
     * Parse the given line (from the game listing) to a IMCSGame object.
     * @param line Line from the game listing to parse.
     * @return The parsed IMCSGame object if all went well, {@code null} otherwise.
     */
    public static IMCSGame parse(String line) {
        Matcher offerMatcher = offerPattern.matcher(line);
        Matcher progressMatcher = progressPattern.matcher(line);
        IMCSGame result = null;

        if(offerMatcher.matches() || progressMatcher.matches()) {
            result = new IMCSGame();
            result.isRunning = progressMatcher.matches();
            if(result.isRunning) {
                result.gameId = progressMatcher.group(1);
            } else {
                result.gameId = offerMatcher.group(1);
                result.reservedPlayer = offerMatcher.group(3).charAt(0);
            }
        }
        return result;
    }

    /* Regular expressions used for parsing of offered and in-progress games */
    private static final Pattern offerPattern =
            Pattern.compile(" (\\d+) (\\w+) (B|W|\\?) (\\d+:\\d{1,2}) (\\d+:\\d{1,2}) (\\d+) \\[offer\\]");
    private static final Pattern progressPattern =
            Pattern.compile(" (\\d+) (\\w+) (\\w+) \\((\\d+)\\/(\\d+)\\)\\s+\\[in-progress\\]");
}
