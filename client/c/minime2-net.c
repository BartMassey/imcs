/* Copyright © 2011 Bart Massey */
/* Please see the end of this file for license information. */

/* This is the code that I use in my current player,
   minime2, to play games over IMCS. Your mileage may
   obviously vary a great deal based on the issues of
   integrating your player. */

void imcsplay(int argc, char **argv) {
    setlinebuf(stdout);
    if (argc < 5 || argc > 7)
	usage();
    char mecolor = '?';
    int megame = 0;
    switch(argv[2][0]) {
    case 'O':
	switch(argv[2][1]) {
	case 'W':
	case 'B':
	case '?':
	    mecolor = argv[2][1];
	    break;
	default:
	    usage();
	}
	break;
    case 'A': {
	char ch = argv[2][1];
	if (isdigit(ch)) {
	    megame = atoi(&argv[2][1]);
	} else if (ch == 'W' || ch == 'B') {
	    mecolor = ch;
	    megame = atoi(&argv[2][2]);
	} else {
	    usage();
	}
	if (megame <= 0)
	    usage();
	break;
    }
    default:
	usage();
    }
    char *meuser = argv[3];
    char *mepassword = argv[4];
    char *host = "imcs.svcs.cs.pdx.edu";
    if (argc > 5)
	host = argv[5];
    int port = 3589;
    if (argc > 6) {
	port = atoi(argv[6]);
	if (port <= 0)
	    usage();
    }
    FILE *nf = netopen(host, port);
    setlinebuf(nf);
    startlog();
    char *greeting = expectcmd(nf, 1, 100, 0);
    (void) strtok(greeting, " ");
    char *pgm = strtok(0, " ");
    assert(!strcmp(pgm, "imcs"));
    char *version = strtok(0, " \r\n");
    if(strcmp(version, "2.5")) {
	fprintf(stderr, "got unexpected imcs version %s\n", version);
	exit(1);
    }
    sendcmd(nf, "me %s %s", meuser, mepassword);
    (void) expectcmd(nf, 1, 201, 0);
    if (megame != 0) {
	if (mecolor == '?')
	    sendcmd(nf, "accept %d", megame);
	else
	    sendcmd(nf, "accept %d %c", megame, mecolor);
	(void) expectcmd(nf, 1, 105, 106, 0);
    } else {
	if (mecolor == '?')
	    sendcmd(nf, "offer");
	else
	    sendcmd(nf, "offer %c", mecolor);
	(void) expectcmd(nf, 1, 103, 0);
	logmsg("waiting for opponent");
	(void) expectcmd(nf, 1, 105, 106, 0);
	logmsg("opponent found");
    }
    struct state s = s0;
    s.cureval = eval(&s);
    if (nttable > 0)
	s.curzhash = zhash(&s);
    while (1) {
	int ch = fgetc(nf);
	int r = ungetc(ch, nf);
	assert(r != EOF);
	if (isdigit(ch)) {
	    s = readstate(nf, 1);
	    s.cureval = eval(&s);
	    if (nttable > 0)
		s.curzhash = zhash(&s);
	    continue;
	}
	switch (ch) {
	case '?': {
	    char *r = getnet(nf, "?");
	    char *q = strtok(r, " ");
	    assert(!strcmp(q, "?"));
	    char *tl = strtok(0, " ");
	    char *tr = strtok(0, " ");
	    assert(tl && tr);
	    int t = readtimems(tl);
	    t = 95 * t / (100 * ((81 - s.ply) / 2));
	    struct move m = idnegamax(&s, t, 0);
	    logmsg("value %d at time %d depth %d for %s\n\n",
		   v0, t, d0, movestr(&m));
	    move(&s, &m, 0);
	    sendcmd(nf, "%s", movestr(&m));
	    printstate(&s, 1);
	    if (ponder)
		(void) idnegamax(&s, 0, nf);
	    continue;
	}
	case '!':
	    assert(fgetc(nf) == '!');
	    int ch;
	    do
		ch = fgetc(nf);
	    while (isspace(ch));
	    ungetc(ch, nf);
	    struct move m = getmove(nf, &s);
	    move(&s, &m, 0);
	    continue;
	case '=':
	    (void) getnet(nf, "=");
	    break;
	case 'X':
	    (void) getnet(nf, "X");
	    break;
	default:
	    (void) getnet(nf, "...");
	    continue;
	}
	break;
    }
    fclose(nf);
}

/*
[This program is licensed under the "MIT License"]

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated
documentation files (the "Software"), to deal in the
Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall
be included in all copies or substantial portions of the
Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
