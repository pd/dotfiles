package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"time"
)

type BuildRequest struct {
	Version  string   `json:"version"`
	Target   string   `json:"target"`
	Profile  string   `json:"profile"`
	Packages []string `json:"packages"`
	Client   string   `json:"client"`
}

type Router struct {
	Version  string
	Target   string
	Profile  string
	Packages []string
}

func (r *Router) BuildRequest() BuildRequest {
	return BuildRequest{
		Version:  r.Version,
		Target:   r.Target,
		Profile:  r.Profile,
		Packages: r.Packages,
		Client:   "github.com/pd",
	}
}

var (
	httpClient = &http.Client{}

	commonPackages = []string{
		"bind-dig",
		"iperf3",
		"luci-app-attendedsysupgrade",
		"luci-app-firewall",
		"luci-mod-admin-full",
		"luci-mod-network",
		"luci-mod-status",
		"luci-mod-system",
		"luci-proto-ipv6",
		"luci-theme-bootstrap",
		"prometheus-node-exporter-lua",
		"prometheus-node-exporter-lua-netstat",
		"prometheus-node-exporter-lua-openwrt",
		"prometheus-node-exporter-lua-wifi",
		"prometheus-node-exporter-lua-wifi_stations",
		"tcpdump",
		"-wpad-basic-mbedtls",
		"wpad",
	}

	routers = map[string]Router{
		"wrt": {
			Version: "24.10.2",
			Target:  "mediatek/filogic",
			Profile: "glinet_gl-mt6000",
			Packages: append(commonPackages, []string{
				"ddns-scripts-digitalocean",
				"luci-app-ddns",
				"luci-app-upnp",
				"prometheus-node-exporter-lua-nat_traffic",
			}...),
		},
		"rpt": {
			Version:  "SNAPSHOT",
			Target:   "qualcommax/ipq50xx",
			Profile:  "glinet_gl-b3000",
			Packages: commonPackages,
		},
	}
)

func request(router *Router) (string, error) {
	payload := router.BuildRequest()
	payloadBytes, err := json.Marshal(payload)
	if err != nil {
		return "", fmt.Errorf("failed to marshal payload: %w", err)
	}

	req, err := http.NewRequest("POST", "https://sysupgrade.openwrt.org/api/v1/build", bytes.NewBuffer(payloadBytes))
	if err != nil {
		return "", fmt.Errorf("failed to create request: %w", err)
	}

	req.Header.Set("Accept", "application/json")
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("User-Agent", "github.com/pd sysupgrade-client")

	resp, err := httpClient.Do(req)
	if err != nil {
		return "", fmt.Errorf("POST build request failed: %w", err)
	}
	defer resp.Body.Close()

	var buildResp struct {
		RequestHash string `json:"request_hash"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&buildResp); err != nil {
		return "", fmt.Errorf("failed to decode POST build response: %w", err)
	}

	return buildResp.RequestHash, nil
}

func waitFor(requestHash string) (string, error) {
	url := fmt.Sprintf("https://sysupgrade.openwrt.org/api/v1/build/%s", requestHash)

	for {
		req, err := http.NewRequest("GET", url, nil)
		if err != nil {
			return "", fmt.Errorf("GET build request failed: %w", err)
		}

		req.Header.Set("Accept", "application/json")
		req.Header.Set("User-Agent", "github.com/pd sysupgrade-client")

		resp, err := httpClient.Do(req)
		if err != nil {
			return "", fmt.Errorf("failed to send request: %w", err)
		}
		defer resp.Body.Close()

		var statusResp struct {
			Status string `json:"imagebuilder_status"`
			Images []struct {
				Type string `json:"type"`
				Name string `json:"name"`
			} `json:"images"`
		}
		if err := json.NewDecoder(resp.Body).Decode(&statusResp); err != nil {
			return "", fmt.Errorf("failed to decode GET build response: %w", err)
		}

		if statusResp.Status != "done" {
			fmt.Fprintf(os.Stderr, "waiting for image to build ...\n")
			time.Sleep(20 * time.Second)
			continue
		}

		for _, img := range statusResp.Images {
			if img.Type == "sysupgrade" {
				return img.Name, nil
			}
		}

		return "", fmt.Errorf("no sysupgrade image found")
	}
}

func download(requestHash, image string) error {
	url := fmt.Sprintf("https://sysupgrade.openwrt.org/store/%s/%s", requestHash, image)

	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return fmt.Errorf("failed to create request: %w", err)
	}

	req.Header.Set("User-Agent", "github.com/pd sysupgrade-client")

	resp, err := httpClient.Do(req)
	if err != nil {
		return fmt.Errorf("GET image request failed: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("GET image failed with status: %s", resp.Status)
	}

	file, err := os.Create(image)
	if err != nil {
		return fmt.Errorf("failed to create file: %s: %w", image, err)
	}
	defer file.Close()

	_, err = io.Copy(file, resp.Body)
	if err != nil {
		return fmt.Errorf("failed to write file: %s: %w", image, err)
	}

	return nil
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintf(os.Stderr, "usage: %s <host>\n", os.Args[0])
		os.Exit(1)
	}

	host := os.Args[1]

	router, ok := routers[host]
	if !ok {
		fmt.Fprintf(os.Stderr, "unknown router: %s\n", host)
		os.Exit(1)
	}

	requestHash, err := request(&router)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to request build: %v\n", err)
		os.Exit(1)
	}

	imagePrefix, err := waitFor(requestHash)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to wait for build: %v\n", err)
		os.Exit(1)
	}

	if err := download(requestHash, imagePrefix); err != nil {
		fmt.Fprintf(os.Stderr, "failed to download image: %v\n", err)
		os.Exit(1)
	}
}
