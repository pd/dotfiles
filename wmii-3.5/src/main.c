/* Writing this practically as two programs launched from the same point. */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include "mpdbar.h"

App   displayer;
App   controller;
char *wmii_address;
char *wmii_normcolors;
char *mpd_host;
int   mpd_port;
int   should_exit = 0;

Bar Bars[] = {
	{0, "state",  "musicpd", ""},
	{1, "volume", "[v:xx /", ""},
	{2, "repeat", "rpt /", ""},
	{3, "random", "rnd]", ""},
	{4, "ldiv",   "[", ""},
	{5, "prev",   "<<", ""},
	{6, "stop",   "X", ""},
	{7, "pause",  ">", ""},
	{8, "next",   ">>", ""},
	{9, "rdiv",   "] |", ""},
	{0, 0, 0, 0}
};

void
fatal(const char *fmt, ...)
{
	va_list ap;
	fprintf(stderr, "mpdbar: error:");
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
	exit(1);
}

void
warn(const char *fmt, ...)
{
	va_list ap;
	fprintf(stderr, "mpdbar: warning:");
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
}

static void
read_config()
{
	char *p;

	wmii_address = getenv("WMII_ADDRESS");
	wmii_normcolors = getenv("WMII_NORMCOLORS");

	if (!wmii_address)
		fatal("WMII_ADDRESS not set");
	if (!wmii_normcolors)
		fatal("WMII_NORMCOLORS not set");

	/* rc's (a b c) lists use  as the delimiter */
	p = wmii_normcolors;
	for (p = wmii_normcolors; *p; p++)
		if (*p == 0x1)
			*p = ' ';

	mpd_host = getenv("MPD_HOST");
	if (!mpd_host)
		mpd_host = "localhost";
	p = getenv("MPD_PORT");
	if (!p)
		mpd_port = 6600;
	else
		mpd_port = (int)strtol(p, NULL, 10);
}

static void
init_app(App *app)
{
	app->ixp = malloc(sizeof(IXPClient));
	app->mpd = malloc(sizeof(MPDClient));
	memset(app->ixp, 0, sizeof(IXPClient));
	memset(app->mpd, 0, sizeof(MPDClient));

	if (wmii_connect(app->ixp, wmii_address) == -1)
		fatal("cannot connect to '%s': %s", wmii_address, app->ixp->errstr);
	if (mpd_connect(app->mpd, mpd_host, mpd_port) == -1)
		fatal("cannot connect to '%s:%d': %s", mpd_host, mpd_port, strerror(errno));
}

/* Child: Reads from /event to respond to bar clicks */
static void
run_controller(App *ctrl)
{
	int i;

	while (usleep(700000) != -1) {
		mpd_get_state(ctrl->mpd, &ctrl->mpd->state);
		wmii_process_events(ctrl);
	}
}

/* Parent: Polls status from mpd and writes to the rbar */
static void
run_displayer(App *dsp)
{
	int i;

	while (usleep(900000) != -1) {
		mpd_get_state(dsp->mpd, &dsp->mpd->state);
		wmii_update_bars(dsp->ixp, &dsp->mpd->state);
	}
}

void
shutdown_app(App *app)
{
	ixp_client_hangup(app->ixp);
	close(app->mpd->fd);
}

static void
sigcatcher(int sig)
{
	switch (sig) {
	case SIGCHLD:
		wmii_remove_bars(displayer.ixp, Bars);
		shutdown_app(&displayer);
		exit(0);
	}
}

int
main(int argc, char *argv[])
{
	Bar *b;
	IXPClient ixp = {0};

	read_config();

	// fill in the paths, didn't feel like typing them
	for (b = Bars; b->name; b++)
		snprintf(b->path, sizeof(b->path)-1, "/rbar/%d_mp3%s", b->pos + 200, b->name);

	// kill any other instances before our child starts reading events
	init_app(&displayer);
	wmii_write_file(displayer.ixp, "/event", "Stop mpdbar");
	init_app(&controller);

	wmii_remove_bars(displayer.ixp, Bars);
	if (wmii_create_bars(displayer.ixp, Bars) == -1)
		fatal("cannot create bars: %s\n", displayer.ixp->errstr);

	/* if our child dies, we should too */
	signal(SIGCHLD, sigcatcher);
	if (fork() == 0)
		run_controller(&controller);
	else
		run_displayer(&displayer);

	shutdown_app(&controller);
	shutdown_app(&displayer);
	return 0;
}
