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
          jellyfin = 8096;
          prom = config.services.prometheus.port;
          rtorrent = 8000;
          synologyDSM = 5000;
        };
      in
      with ports;
      {
        "alerts.home:80" = "localhost:${alertmanager}";
        "grafana.home:80" = "localhost:${grafana}";
        "jellyfin.home:80" = "localhost:${jellyfin}";
        "jf.krh.me" = "localhost:${jellyfin}";
        "prom.home:80" = "localhost:${prom}";
        "store.home:80" = "nas.home:${synologyDSM}";
        "torrent.home:80".handlers = [
          {
            route = "/_hooks/*";
            upstream = "localhost:${filebotd}";
          }
          {
            route = "";
            upstream = "nas.home:${rtorrent}";
          }
        ];
      };
  };
}
