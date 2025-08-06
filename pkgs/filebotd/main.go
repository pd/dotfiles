package main

import (
	"errors"
	"fmt"
	"log/slog"
	"net/http"
	"net/url"
	"os"
	"os/exec"
	"regexp"
)

// Tiny server responsible for running filebot as an after-completion
// hook fired from rtorrent on nas. Runs on htpc cuz it's easier to
// provision.
func main() {
	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":12345"
	}

	httpClient := &http.Client{}

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

		if role == "archive" || role == "opsbetter" {
			w.WriteHeader(http.StatusOK)
			log.Info("skipping torrent", "role", role)
			return
		}

		kind, ok := map[string]string{
			"landof.tv":         "tv",
			"opsfet.ch":         "music",
			"passthepopcorn.me": "movie",
		}[tracker]
		if !ok {
			w.WriteHeader(http.StatusBadRequest)
			log.Error("unsupported tracker", "status", http.StatusBadRequest)
			return
		}

		cmd := mediaSort(path, kind)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr

		log.Info("running media-sort", "cmd", cmd.Args)
		err := cmd.Run()
		log.Info("filebot exited", "error", err)

		go func() {
			if err := scanLibrary(httpClient, kind); err != nil {
				log.Error("jellyfin refresh failed", "error", err)
			}
		}()

		// We always 200 cuz filebot exits non-zero for all kinds of reasons
		// and who cares really
		w.WriteHeader(http.StatusOK)
	})

	if err := http.ListenAndServe(addr, mux); err != nil {
		slog.Error("ListenAndServe", "error", err)
	}
}

func mediaSort(src, kind string) *exec.Cmd {
	return exec.Command(
		"media-sort",
		kind,
		src,
	)
}

func scanLibrary(c *http.Client, kind string) error {
	jfURL := os.Getenv("JELLYFIN_URL")
	jfKey := os.Getenv("JELLYFIN_API_KEY")

	if jfURL == "" || jfKey == "" {
		return errors.New("JELLYFIN_URL or JELLYFIN_API_KEY unset")
	}

	libraryID := map[string]string{
		"movie": "f137a2dd21bbc1b99aa5c0f6bf02a805",
		"music": "7e64e319657a9516ec78490da03edccb",
		"tv":    "a656b907eb3a73532e40e44b968d0225",
	}[kind]

	req, err := http.NewRequest("POST", fmt.Sprintf("%s/Items/%s/Refresh", jfURL, libraryID), nil)
	if err != nil {
		return fmt.Errorf("creating jellyfin refresh request: %w", err)
	}

	req.Header.Set("X-MediaBrowser-Token", jfKey)
	q := url.Values{}
	for k, v := range map[string]string{
		"Recursive":           "true",
		"ImageRefreshMode":    "Default",
		"MetadataRefreshMode": "Default",
		"ReplaceAllImages":    "false",
		"RegenerateTrickplay": "false",
		"ReplaceAllMetadata":  "false",
	} {
		q.Add(k, v)
	}
	req.URL.RawQuery = q.Encode()

	resp, err := c.Do(req)
	if err != nil {
		return fmt.Errorf("posting jellyfin refresh request: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusNoContent {
		return fmt.Errorf("posting jellyfin refresh request responded %d", resp.StatusCode)
	}

	return nil
}

// rtorrent on nas sees `/downloads/...`, everything else
// sees `/media/torrents/...`
var rgxDownloads = regexp.MustCompile(`^/downloads/`)

func rewriteDataPath(path string) string {
	return rgxDownloads.ReplaceAllLiteralString(path, "/media/torrents/")
}
