{ ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nameserver
    ./prometheus
    ./arr.nix
    ./grafana.nix
    ./harmonia.nix
    ./httpd.nix
    ./immich.nix
    ./jellyfin.nix
    ./nas.nix
    ./psql.nix
    ./scrobble.nix
  ];

  system.stateVersion = "24.05";
  time.timeZone = "America/Chicago";
  docs.enable = false;

  networking.hostName = "htpc";
  lan.wired.interface = "eno1";
  lan.wifi.interface = "wlan0";
  lan.wifi.id = {
    duid = "de:ad:c0:de:ca:fe:00:82";
    iaid = 18631;
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.enableRedistributableFirmware = true;
  security.rtkit.enable = true;

  virtualisation.quadlet = {
    enable = true;
    autoEscape = true;
  };

  services.keyd = {
    enable = true;
    keyboards = {
      default = {
        ids = [ "*" ];
        settings = {
          main = {
            capslock = "layer(control)";
          };
        };
      };
    };
  };

  monitoring.heartbeat = true;
  monitoring.processes = {
    caddy.comm = [ "caddy" ];
    dnsmasq.comm = [ "dnsmasq" ];
    grafana.comm = [ "grafana" ];
    harmonia.comm = [ "harmonia-cache" ];
    immich.comm = [ "immich" ];
    jellyfin.comm = [ "jellyfin" ];
    lidarr.cmdline = [ "^.+Lidarr.dll" ];
    radarr.comm = [ "Radarr" ];
    postgres.comm = [ "postgres" ];
    prowlarr.comm = [ "Prowlarr" ];
    prometheus.comm = [ "prometheus" ];
    qbittorrent.cmdline = [ "^/[^ ]+/bin/qbittorrent-nox" ];
    sonarr.comm = [ "Sonarr" ];
  };
}
