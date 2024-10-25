package main

import (
	"crypto/md5"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"
)

// The rtorrentrc side of the config to work with this, assuming this
// is exposed at torrent.home/directorrent --

/*
method.insert = move_media_dest_path, simple,\
  "execute.capture = curl, --silent, -G,\
	--data-urlencode, (cat,\"data-path=\",(d.data_path)),\
	--data-urlencode, (cat,\"tracker=\",(d.custom1)),\
	http://torrent.home/directorrent/on-completion"

method.insert = media_dirname, simple|private, \
	"execute.capture = bash, -c, \"dirname \\\"$1\\\" | tr -d $'\\\\n'\", \
							 media_dirname, (argument.0)"

method.insert = move_media_single, simple, \
  "d.directory.set = (argument.1); \
   execute.throw = mkdir, -p, (argument.1); \
   execute.throw = mv, -u, (argument.0), (argument.1)"

method.insert = move_media_multi, simple, \
  "d.directory_base.set = (argument.1); \
   execute.throw = mkdir, -p, (media_dirname, (argument.1)); \
   execute.throw = mv, -uT, (argument.0), (argument.1)"

method.insert = move_media, simple, \
  "branch=d.is_multi_file=, \
	\"move_media_multi = (argument.0), (argument.1)\", \
	\"move_media_single = (argument.0), (argument.1)\" ; \
  d.save_full_session="

method.insert = move_media_safe, simple,\
  "branch=\"not=(equal, argument.0=, cat=)\", \
	\"move_media = (d.base_path), (argument.0)\""

method.set_key = event.download.finished, move_on_completion, \
  "move_media_safe = (move_media_dest_path)"
*/

func main() {
	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":12345"
	}

	mux := http.NewServeMux()
	mux.Handle("GET /on-completion", http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		q := r.URL.Query()
		dataPath := q.Get("data-path")
		tracker := q.Get("tracker")

		sum := md5.Sum([]byte(tracker))
		trackerID := hex.EncodeToString(sum[:])

		md := map[string]any{
			"url": r.URL.String(),
			"dataPath":  dataPath,
			"tracker":   tracker,
			"trackerID": trackerID,
		}
		defer log(md)

		// Already in /media, nothing to do
		if strings.HasPrefix(dataPath, "/media") {
			md["status"] = http.StatusNoContent
			w.WriteHeader(http.StatusNoContent)
			return
		}

		dest, ok := map[string]string{
			"99970f674456d6e4275a003c400df826": "Movies",
			"a26de5044b6712ae335cd7b46dd6d090": "TV",
			"7e4a66878a7120870d4103495603d430": "TV",
		}[trackerID]
		if !ok {
			md["status"] = http.StatusNotFound
			w.WriteHeader(http.StatusNotFound)
			return
		}

		// trailing `/` => multifile
		var out string
		if strings.HasSuffix(dataPath, "/") {
			out = fmt.Sprintf("/media/%s/%s", dest, filepath.Base(dataPath))
		} else {
			out = fmt.Sprintf("/media/%s", dest)
		}

		md["status"] = http.StatusOK
		md["out"] = out
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(out))
	}))

	mux.Handle("GET /", http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		log(map[string]any{
			"status": http.StatusNotFound,
			"url": r.URL.String(),
		})
		w.WriteHeader(http.StatusNotFound)
	}))

	if err := http.ListenAndServe(addr, mux); err != nil {
		fmt.Fprintln(os.Stderr, "ListenAndServe:", err)
		os.Exit(1)
	}
}

func log(data map[string]any) {
	data["time"] = time.Now().Truncate(time.Second).Format(time.RFC3339)
	json.NewEncoder(os.Stdout).Encode(data)
}
