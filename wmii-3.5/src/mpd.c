#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include "mpdbar.h"

static int
mpd_request_state(MPDClient *mc)
{
	return mpd_send(mc, "status\ncurrentsong");
}

static int
mpd_extract_state(MPDClient *mc, MPDState *s)
{
	char buf[4096];
	char *tok, *endp;
	int count;

	memset(buf, 0, sizeof(buf));
	memset(s, 0, sizeof(MPDState));
	if ((count = read(mc->fd, buf, sizeof(buf)-1)) == -1)
		return -1;

	for (tok = strtok(buf, "\n"); tok; tok = strtok(NULL, "\n")) {
		if (!strncmp(tok, "OK", 2))
			continue;
		else if (!strncmp(tok, "volume: ", 8))
			s->volume = (int)strtol(tok+8, NULL, 10);
		else if (!strncmp(tok, "repeat: ", 8))
			s->repeat = (int)strtol(tok+8, NULL, 10);
		else if (!strncmp(tok, "random: ", 8))
			s->random = (int)strtol(tok+8, NULL, 10);
		else if (!strncmp(tok, "state: ", 7)) {
			if (!strncmp(tok+7, "play", 4))
				s->state = MPD_PLAY;
			else if (!strncmp(tok+7, "pause", 5))
				s->state = MPD_PAUSE;
			else if (!strncmp(tok+7, "stop", 4))
				s->state = MPD_STOP;
		}
		else if (!strncmp(tok, "time: ", 6)) {
			s->elapsed = (int)strtol(tok+6, &endp, 10);
			s->total = (int)strtol(++endp, NULL, 10);
		}
		else if (!strncmp(tok, "file: ", 6))
			strncpy(s->file, tok+6, sizeof(s->file));
		else if (!strncmp(tok, "Artist: ", 8))
			strncpy(s->artist, tok+8, sizeof(s->artist));
		else if (!strncmp(tok, "Title: ", 7))
			strncpy(s->title, tok+7, sizeof(s->title));
		else if (!strncmp(tok, "Album: ", 7))
			strncpy(s->album, tok+7, sizeof(s->album));
		else if (!strncmp(tok, "Track: ", 7))
			strncpy(s->track, tok+7, sizeof(s->track));
		else if (!strncmp(tok, "Pos: ", 5))
			s->pos = (int)strtol(tok+5, NULL, 10);
		else if (!strncmp(tok, "Id: ", 4))
			s->id = (int)strtol(tok+4, NULL, 10);
	}

	return 0;
}

int
mpd_connect(MPDClient *mc, const char *address, int port)
{
	struct sockaddr_in sin;
	struct hostent *he;

	if ((mc->fd = socket(PF_INET, SOCK_STREAM, 0)) == -1)
		return -1;
	if ((he = gethostbyname(address)) == NULL)
		return -1;

	memset(&sin, 0, sizeof(struct sockaddr_in));
	sin.sin_family = AF_INET;
	memcpy(&sin.sin_addr.s_addr, he->h_addr, he->h_length);
	sin.sin_port = htons(port);

	return connect(mc->fd, (struct sockaddr *)&sin, sizeof(struct sockaddr));
}

int
mpd_send(MPDClient *mc, const char *data)
{
	char buf[1024];
	snprintf(buf, sizeof(buf)-1, "command_list_begin\n%s\ncommand_list_end\n", data);
	return write(mc->fd, buf, strlen(buf));
}

int
mpd_ping(MPDClient *mc)
{
	return mpd_send(mc, "ping");
}

int
mpd_get_state(MPDClient *mc, MPDState *s)
{
	if (mpd_request_state(mc) == -1)
		return -1;
	return mpd_extract_state(mc, s);
}
