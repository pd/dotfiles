#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "mpdbar.h"

static int
wmii_create_file(IXPClient *ixpc, const char *path)
{
	unsigned int fid = ixpc->root_fid << 2;
	char *p, fname[512];
	int ret = 0;

	strncpy(fname, path, sizeof(fname)-1);
	p = strrchr(fname, '/');

	*p = 0;
	if (ixp_client_walk(ixpc, fid, fname) == -1)
		return -1;

	p++;
	if (ixp_client_create(ixpc, fid, p, IXP_DMWRITE, IXP_OWRITE) == -1)
		ret = -1;

	ixp_client_close(ixpc, fid);
	return ret;
}

static int
wmii_remove_file(IXPClient *ixpc, const char *path)
{
	char fname[512];
	strncpy(fname, path, sizeof(fname)-1);
	return ixp_client_remove(ixpc, ixpc->root_fid << 2, fname);
}

int
wmii_write_file(IXPClient *ixpc, const char *path, char *data)
{
	unsigned int fid = ixpc->root_fid << 2;
	char fname[512];
	int ret;

	strncpy(fname, path, sizeof(fname)-1);
	if (ixp_client_walkopen(ixpc, fid, fname, IXP_OWRITE) == -1)
		return -1;

	if (ixp_client_write(ixpc, fid, 0, strlen(data), data) != strlen(data))
		ret = -1;

	ixp_client_close(ixpc, fid);
	return ret;
}

int
wmii_write_bar(IXPClient *ixpc, const char *bar, const char *data)
{
	Bar *b;
	char buf[1024];

	snprintf(buf, sizeof(buf)-1, "%s %s", wmii_normcolors, data);
	for (b = Bars; b->name; b++)
		if (!strncmp(b->name, bar, strlen(bar)))
			break;
	if (!b->name)
		return -1;

	return wmii_write_file(ixpc, b->path, buf);
}

int
wmii_connect(IXPClient *ixpc, const char *address)
{
	char sname[512];
	strncpy(sname, address, sizeof(sname)-1);
	return ixp_client_dial(ixpc, sname, getpid());
}

int
wmii_create_bars(IXPClient *ixpc, Bar *bars)
{
	Bar *b;
	char data[64];

	for (b = bars; b->name; b++) {
		snprintf(data, sizeof(data)-1, "%s %s", wmii_normcolors, b->contents);
		if (wmii_create_file(ixpc, b->path) == -1)
			return -1;
		if (wmii_write_file(ixpc, b->path, data) == -1)
			return -1;
	}

	return 0;
}

void
wmii_remove_bars(IXPClient *ixpc, Bar *bars)
{
	Bar *b;
	for (b = bars; b->name; b++)
		wmii_remove_file(ixpc, b->path);
}

static void
wmii_barclick(MPDClient *mc, int button, const char *barname)
{
	MPDState *s = &mc->state;
	char buf[32];

	// skip the 2xx_mp3
	barname += 7;
	if (!strncmp(barname, "volume", 6)) {
		snprintf(buf, sizeof(buf)-1, "setvol %d",
		         s->volume + (button == 1 ? -5 : 5));
		mpd_send(mc, buf);
	}
	else if (!strncmp(barname, "repeat", 6)) {
		snprintf(buf, sizeof(buf)-1, "repeat %d",
		         s->repeat ? 0 : 1);
		mpd_send(mc, buf);
	}
	else if (!strncmp(barname, "random", 6)) {
		snprintf(buf, sizeof(buf)-1, "random %d",
		         s->random ? 0 : 1);
		mpd_send(mc, buf);
	}
	else if (!strncmp(barname, "prev", 4))
		mpd_send(mc, "previous");
	else if (!strncmp(barname, "stop", 4))
		mpd_send(mc, "stop");
	else if (!strncmp(barname, "pause", 5)) {
		switch (s->state) {
		case MPD_PLAY:
			mpd_send(mc, "pause 1");
			break;
		case MPD_PAUSE:
			mpd_send(mc, "pause 0");
			break;
		case MPD_STOP:
			mpd_send(mc, "play");
			break;
		}
	}
	else if (!strncmp(barname, "next", 4))
		mpd_send(mc, "next");
}

void
wmii_process_events(App *app)
{
	IXPClient *ixpc = app->ixp;
	MPDClient *mpdc = app->mpd;
	unsigned int fid = ixpc->root_fid << 2;
	unsigned char buf[IXP_MAX_MSG];
	char *endp, *barname;
	int button;
	int count;

	if (ixp_client_walkopen(ixpc, fid, "/event", IXP_OREAD) == -1)
		fatal("cannot open '/event' for reading: %s", ixpc->errstr);

	if ((count = ixp_client_read(ixpc, fid, 0, buf, IXP_MAX_MSG)) > 0) {
		buf[count] = 0;
		if (!strncmp(buf, "Stop mpdbar", 11)) {
			shutdown_app(app);
			exit(0);
		} else if (!strncmp(buf, "RightBarClick ", 14)) {
			button = (int)strtol(buf+14, &endp, 10);
			barname = ++endp;
			wmii_barclick(mpdc, button, barname);
		}
	}

	if (count == -1)
		fatal("cannot read '/event': %s\n", ixpc->errstr);

	ixp_client_close(ixpc, fid);
}

void
wmii_update_bars(IXPClient *ixpc, MPDState *s)
{
	char buf[512];

	if (s->state == MPD_STOP)
		wmii_write_bar(ixpc, "state", "not playing");
	else if (s->state == MPD_PLAY || s->state == MPD_PAUSE) {
		snprintf(buf, sizeof(buf)-1, "%s: %s - %s - %s - %s [%d:%02d/%d:%02d]",
		         s->state == MPD_PLAY ? "np" : "paused",
		         s->artist, s->album, s->track, s->title,
				 s->elapsed / 60, s->elapsed % 60,
		         s->total   / 60, s->total   % 60);
		wmii_write_bar(ixpc, "state", buf);
	}

	snprintf(buf, sizeof(buf)-1, "[v:%02d /", s->volume);
	wmii_write_bar(ixpc, "volume", buf);
	wmii_write_bar(ixpc, "repeat", s->repeat ? "RPT /" : "rpt /");
	wmii_write_bar(ixpc, "random", s->random ? "RND]" : "rnd]");
}
