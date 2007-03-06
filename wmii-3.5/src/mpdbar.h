#include <stdarg.h>
#include <ixp.h>

typedef struct wmii_bar {
	int  pos;
	char *name;
	char *contents;
	char path[64];
} Bar;

enum {
	MPD_STOP,
	MPD_PAUSE,
	MPD_PLAY,
};

typedef struct mpd_state {
	/* status */
	int volume;
	int repeat;
	int random;
	int state;
	int elapsed;
	int total;

	/* currentsong -- stupidly large but meh */
	char file[2048];
	char artist[256];
	char title[256];
	char album[256];
	char track[256];
	int pos;
	int id;
} MPDState;

typedef struct mpd_client {
	int fd;
	MPDState state;
} MPDClient;

typedef struct mpdbar_app {
	IXPClient *ixp;
	MPDClient *mpd;
} App;

extern Bar   Bars[];
extern char *wmii_normcolors;
extern char *wmii_address;
extern char *mpd_host;
extern int   mpd_port;
extern int   should_exit;

void fatal(const char *, ...);
void warn(const char *, ...);
void shutdown_app(App *);

int  mpd_connect(MPDClient *, const char *, int);
int  mpd_ping(MPDClient *);
int  mpd_send(MPDClient *, const char *);
int  mpd_get_state(MPDClient *, MPDState *);

int  wmii_connect(IXPClient *, const char *);
int  wmii_create_bars(IXPClient *, Bar *);
void wmii_remove_bars(IXPClient *, Bar *);
int  wmii_write_file(IXPClient *, const char *, char *);
void wmii_process_events(App *);
void wmii_update_bars(IXPClient *, MPDState *);
