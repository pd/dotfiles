SRC = main.c wmii.c mpd.c
OBJ = ${SRC:.c=.o}

CFLAGS = -I/usr/local/include -I.
LDFLAGS = -L/usr/local/lib -lixp

all: mpdbar

.c.o:
	${CC} -c ${CFLAGS} $<

mpdbar: ${OBJ}
	${CC} -o $@ ${OBJ} ${LDFLAGS}

install: mpdbar
	cp mpdbar ${HOME}/.wmii-3.5
	chmod 755 ${HOME}/.wmii-3.5/mpdbar

clean:
	rm -f mpdbar *.core ${OBJ}
