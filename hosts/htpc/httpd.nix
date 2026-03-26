{ config, lib, ... }:
{
  imports = [ ../../modules/httpd ];

  httpd = {
    enable = true;
    reverseProxies =
      let
        ports = lib.mapAttrs (_: port: toString port) {
          alertmanager = config.services.prometheus.alertmanager.port;
          filebotd = 12345;
          grafana = config.services.grafana.settings.server.http_port;
          harmonia = 5000;
          immich = config.services.immich.port;
          jellyfin = 8096;
          koito = 4110;
          lidarr = config.services.lidarr.settings.server.port;
          multiscrobbler = 9078;
          npd = 9776;
          prom = config.services.prometheus.port;
          prowlarr = config.services.prowlarr.settings.server.port;
          qbittorrent = config.services.qbittorrent.webuiPort;
          radarr = config.services.radarr.settings.server.port;
          seerr = 5055;
          sonarr = config.services.sonarr.settings.server.port;
          synologyDSM = 5000;
        };
      in
      with ports;
      {
        "alerts.home:80" = "localhost:${alertmanager}";
        "grafana.home:80" = "localhost:${grafana}";
        "img.home:80" = "localhost:${immich}";
        "jellyfin.home:80" = "localhost:${jellyfin}";
        "jf.krh.me" = "localhost:${jellyfin}";
        "koito.home:80" = "localhost:${koito}";
        "lidarr.home:80" = "localhost:${lidarr}";
        "nix.home:80" = "localhost:${harmonia}";
        "npd.krh.me" = "localhost:${npd}";
        "prom.home:80" = "localhost:${prom}";
        "prowlarr.home:80" = "localhost:${prowlarr}";
        "radarr.home:80" = "localhost:${radarr}";
        "rr.krh.me" = "localhost:${seerr}";
        "scrobbler.home:80" = "localhost:${multiscrobbler}";
        "seerr.home:80" = "localhost:${seerr}";
        "sonarr.home:80" = "localhost:${sonarr}";
        "store.home:80" = "nas.home:${synologyDSM}";
        "torrent.home:80".handlers = [
          {
            route = "/_hooks/*";
            upstream = "localhost:${filebotd}";
          }
          {
            route = "";
            upstream = "localhost:${qbittorrent}";
          }
        ];
      };
  };
}
