/* Copyright © 2011 Bart Massey */
/* Please see the end of this file for license information. */

#include <assert.h>
#include <netdb.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include "netops.h"

FILE *netopen(char *host, int port) {
    struct hostent *hp = gethostbyname(host);
    if (!hp) {
	herror("gethostbyname");
	exit(1);
    }
    int s = socket(hp->h_addrtype, SOCK_STREAM, 0);
    if (s == -1) {
	perror("socket");
	exit(1);
    }
    char **a;
    for (a = hp->h_addr_list; *a; a++) {
	struct sockaddr_in sa;
	sa.sin_family = hp->h_addrtype;
	sa.sin_port = htons(port);
	memcpy(&sa.sin_addr, *a, hp->h_length);
	if (connect(s, (struct sockaddr *)&sa, sizeof(sa)) != -1)
	    break;
    }
    if (!*a) {
	perror("connect");
	exit(1);
    }
    FILE *f = fdopen(s, "a+");
    if (!f) {
	perror("fdopen");
	exit(1);
    }
    setlinebuf(f);
    return f;
}

static char buf[1024];

FILE *logfile;

void startlog(void) {
    logfile = fopen("minime2.log", "w");
    if (!logfile) {
	perror("logfile");
	exit(1);
    }
    setlinebuf(logfile);
}

char *getnet(FILE *nf, char *state) {
    char *r = fgets(buf, sizeof(buf), nf);
    if (!r || strlen(r) >= sizeof(buf) - 1) {
	perror("fgets");
	fprintf(stderr, "fgets fails in state '%s'\n", state);
	exit(1);
    }
    printf("%s", r);
    if (logfile)
	fprintf(logfile, "%s", r);
    return r;
}

char *expectcmd(FILE *s, int fail, ...) {
    char *r = fgets(buf, sizeof(buf), s);
    if (r == 0 || strlen(buf) >= sizeof(buf) - 1) {
	perror("fgets");
	exit(1);
    }
    printf("%s", buf);
    if (logfile)
	fprintf(logfile, "%s", buf);
    va_list args;
    va_start(args, fail);
    int code;
    while ((code = va_arg(args, int)) != 0) {
	assert(code > 0 && code <= 999);
	char cstr[4];
	sprintf(cstr, "%d", code);
	if (!strncmp(cstr, buf, 3)) {
	    va_end(args);
	    return buf;
	}
    }
    va_end(args);
    if (fail) {
	logmsg("expect: got unexpected code in '%s'", buf);
	exit(1);
    }
    return 0;
}

void logmsg(char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
    printf("\n");
    if (logfile) {
	va_start(args, fmt);
	vfprintf(logfile, fmt, args);
	va_end(args);
	fprintf(logfile, "\n");
    }
}

void logchar(char ch) {
    putchar(ch);
    if (logfile)
	fputc(ch, logfile);
}

void sendcmd(FILE *s, char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(s, fmt, args);
    fprintf(s, "\r\n");
    va_end(args);
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
    printf("\n");
    if (logfile) {
	va_start(args, fmt);
	vfprintf(logfile, fmt, args);
	va_end(args);
	fprintf(logfile, "\n");
    }
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
