package main

import (
	"context"
	_ "embed"
	"encoding/json"
	"fmt"
	"html/template"
	"io"
	"net/http"
	neturl "net/url"
	"os"
	"slices"
	"time"
)

//go:embed index.html.tmpl
var tplIndex string

//go:embed favicon.svg
var favicon []byte

type JF struct {
	client *http.Client
	url    string
	apiKey string
	userID string
}

type Session struct {
	UserId    string
	PlayState struct {
		IsPaused bool
	}
	NowPlayingItem *struct {
		Id      string
		AlbumId string
		Artist  string `json:"AlbumArtist"`
		Album   string
		Name    string
	}
}

type Album struct {
	LastPlayed time.Time
	AlbumId    string
	Artist     string
	Album      string
}

func (a *Album) Image() string {
	return fmt.Sprintf("/img/%s", a.AlbumId)
}

func (a *Album) ImageAbs() string {
	// just never change hostnames, it's fine
	return fmt.Sprintf("https://npd.krh.me%s", a.Image())
}

func main() {
	if err := mainErr(context.Background()); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func mainErr(_ context.Context) error {
	addr, err := getenv("ADDR")
	if err != nil {
		return err
	}

	url, err := getenv("JELLYFIN_URL")
	if err != nil {
		return err
	}

	apiKey, err := getenv("JELLYFIN_APIKEY")
	if err != nil {
		return err
	}

	jf, err := NewJF(url, apiKey)
	if err != nil {
		return err
	}

	tpl := template.Must(template.New("index").Parse(tplIndex))

	srv := http.NewServeMux()
	srv.Handle("GET /favicon.svg", http.HandlerFunc(icon))
	srv.Handle("GET /img/{id}", imageProxy(jf))
	srv.Handle("GET /", index(jf, tpl))

	return http.ListenAndServe(addr, srv)
}

func getenv(key string) (string, error) {
	v := os.Getenv(key)
	if v == "" {
		return "", fmt.Errorf("$%s unset", key)
	}
	return v, nil
}

func icon(w http.ResponseWriter, _ *http.Request) {
	w.Header().Set("Content-Type", "image/svg+xml")
	w.Write(favicon)
}

func index(jf *JF, tpl *template.Template) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		data := struct {
			NP     *Album
			Recent []*Album
		}{}

		np, err := jf.Session()
		if err != nil {
			fmt.Fprintln(os.Stderr, "index:", err)
			w.WriteHeader(http.StatusInternalServerError)
			return
		}

		if np.NowPlayingItem != nil && !np.PlayState.IsPaused {
			data.NP = &Album{
				AlbumId: np.NowPlayingItem.AlbumId,
				Artist:  np.NowPlayingItem.Artist,
				Album:   np.NowPlayingItem.Album,
			}
		}

		recent, err := jf.RecentAlbums()
		if err != nil {
			fmt.Fprintln(os.Stderr, "index:", err)
			w.WriteHeader(http.StatusInternalServerError)
			return
		}

		if len(recent) > 0 && data.NP != nil {
			if recent[0].Album == data.NP.Album && recent[0].Artist == data.NP.Artist {
				recent = recent[1:]
			}
		}

		data.Recent = recent[:min(len(recent), 16)]

		if err := tpl.Execute(w, data); err != nil {
			// too late to 500
			fmt.Fprintln(os.Stderr, "index:", err)
			return
		}
	})
}

func imageProxy(jf *JF) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		err := jf.proxy(fmt.Sprintf("/Items/%s/Images/Primary", r.PathValue("id")), r.URL.Query(), w)
		if err != nil {
			fmt.Fprintln(os.Stderr, "imageProxy:", err)
			return
		}
	})
}

func NewJF(url, apiKey string) (*JF, error) {
	client := &http.Client{}
	jf := &JF{client: client, url: url, apiKey: apiKey}

	np, err := jf.Session()
	if err != nil {
		return nil, fmt.Errorf("Jellyfin user discovery: %w", err)
	}

	jf.userID = np.UserId
	return jf, nil
}

func (jf *JF) Session() (*Session, error) {
	var sessions []*Session
	if err := jf.get("/Sessions", nil, &sessions); err != nil {
		return nil, fmt.Errorf("GetSession: %w", err)
	}

	if len(sessions) == 0 {
		return nil, fmt.Errorf("GetSession: empty result set")
	}

	// Take anything, but prefer something now playing
	session := sessions[0]
	for _, s := range sessions {
		if s.NowPlayingItem != nil && s.PlayState.IsPaused == false {
			session = s
			break
		}
	}

	return session, nil
}

func (jf *JF) RecentAlbums() ([]*Album, error) {
	response := struct {
		Items []struct {
			AlbumId     string
			Album       string
			AlbumArtist string
			UserData    struct {
				LastPlayedDate time.Time
			}
		}
	}{}

	err := jf.get(fmt.Sprintf("/Users/%s/Items", jf.userID), neturl.Values{
		"Recursive":        {"true"},
		"IncludeItemTypes": {"Audio"},
		"Filters":          {"IsPlayed"},
		"SortBy":           {"DatePlayed"},
		"SortOrder":        {"Descending"},
		"Limit":            {"400"}, // to hopefully get at least 12 albums back
	}, &response)
	if err != nil {
		return nil, fmt.Errorf("RecentAlbums: %w", err)
	}

	byId := map[string]*Album{}
	for _, item := range response.Items {
		album, ok := byId[item.AlbumId]
		if !ok {
			album = &Album{
				AlbumId:    item.AlbumId,
				Artist:     item.AlbumArtist,
				Album:      item.Album,
				LastPlayed: item.UserData.LastPlayedDate,
			}
			byId[item.AlbumId] = album
		} else if item.UserData.LastPlayedDate.After(album.LastPlayed) {
			byId[item.AlbumId].LastPlayed = item.UserData.LastPlayedDate
		}
	}

	latest := make([]*Album, 0, len(byId))
	for _, album := range byId {
		latest = append(latest, album)
	}

	slices.SortFunc(latest, func(a *Album, b *Album) int {
		return b.LastPlayed.Compare(a.LastPlayed)
	})

	return latest, nil
}

func (jf *JF) get(path string, q neturl.Values, out any) error {
	req, err := http.NewRequest("GET", fmt.Sprintf("%s%s", jf.url, path), nil)
	if err != nil {
		return fmt.Errorf("GET %s: %w", path, err)
	}

	req.Header.Set("Authorization", fmt.Sprintf("MediaBrowser Token=%s", jf.apiKey))
	req.URL.RawQuery = q.Encode()

	resp, err := jf.client.Do(req)
	if err != nil {
		return fmt.Errorf("GET %s: %w", path, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("GET %s: Jellyfin returned %d: %s", path, resp.StatusCode, string(body))
	}

	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil {
		return fmt.Errorf("decoding /Sessions: %w", err)
	}

	return nil
}

func (jf *JF) proxy(path string, q neturl.Values, w io.Writer) error {
	req, err := http.NewRequest("GET", fmt.Sprintf("%s%s", jf.url, path), nil)
	if err != nil {
		return fmt.Errorf("GET %s: %w", path, err)
	}

	req.URL.RawQuery = q.Encode()

	resp, err := jf.client.Do(req)
	if err != nil {
		return fmt.Errorf("GET %s: %w", path, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("GET %s: Jellyfin returned %d: %s", path, resp.StatusCode, string(body))
	}

	_, err = io.Copy(w, resp.Body)
	return err
}
