package main

import (
	"log/slog"
	"net/http"
	"os"
	"os/exec"
	"regexp"
	"strings"
)

// Tiny server responsible for running filebot as an after-completion
// hook fired from rtorrent on nas. Runs on htpc cuz it's easier to
// provision.
func main() {
	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":12345"
	}

	mux := http.NewServeMux()
	mux.HandleFunc("POST /_hooks/completed", func(w http.ResponseWriter, r *http.Request) {
		tracker := r.PostFormValue("tracker")
		path := rewriteDataPath(r.PostFormValue("path"))
		role := r.PostFormValue("role")

		log := slog.With("tracker", tracker, "path", path)
		if tracker == "" || path == "" {
			w.WriteHeader(http.StatusBadRequest)
			log.Error("empty params", "status", http.StatusBadRequest)
			return
		}

		if role == "archive" {
			w.WriteHeader(http.StatusOK)
			log.Info("skipping archive torrent")
			return
		}

		if role == "opsbetter" {
			w.WriteHeader(http.StatusOK)
			log.Info("skipping opsbetter upload")
			return
		}

		var dest string
		switch tracker {
		case "passthepopcorn.me":
			dest = "/media/sorted/Movies"
		case "landof.tv":
			dest = "/media/sorted/TV"
		default:
			w.WriteHeader(http.StatusBadRequest)
			log.Error("unsupported tracker", "status", http.StatusBadRequest)
			return
		}

		cmd := filebot(path, dest)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr

		log.Info("running filebot", "cmd", cmd.Args)
		err := cmd.Run()
		log.Info("filebot exited", "error", err)

		// We always 200 cuz filebot exits non-zero for all kinds of reasons
		// and who cares really
		w.WriteHeader(http.StatusOK)
	})

	if err := http.ListenAndServe(addr, mux); err != nil {
		slog.Error("ListenAndServe", "error", err)
	}
}

func filebot(src, dest string) *exec.Cmd {
	db := "TheMovieDB"
	if strings.HasSuffix(dest, "/TV") {
		db = "TheMovieDB::TV"
	}

	return exec.Command(
		"filebot",
		"-rename",
		"-non-strict",
		"-no-xattr",
		"--action", "symlink",
		"--conflict", "skip",
		"--format", "{jellyfin.tail}",
		"--db", db,
		"-r", src,
		"--output", dest,
	)
}

// rtorrent on nas sees `/downloads/...`, everything else
// sees `/media/torrents/...`
var rgxDownloads = regexp.MustCompile(`^/downloads/`)

func rewriteDataPath(path string) string {
	return rgxDownloads.ReplaceAllLiteralString(path, "/media/torrents/")
}
