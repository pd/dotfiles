{ config, ... }:
let
  inherit (builtins) map toString;

  targets = port: names: map (n: "${n}:${toString port}") names;
  staticJob = job_name: port: hosts: {
    inherit job_name;
    scrape_interval = "60s";
    static_configs = [ { targets = targets port hosts; } ];
  };

  ports =
    let
      prometheus = config.services.prometheus;
      exporters = prometheus.exporters;
    in
    {
      jellyfin = 8096;
      nginx-exporter = exporters.nginx.port;
      node-exporter = exporters.node.port;
      prometheus = prometheus.port;
      wireguard = exporters.wireguard.port;
    };
in
{
  services.prometheus = {
    enable = true;
    enableReload = true;
    webExternalUrl = "http://prom.home";
    port = 9999;
    exporters = {
      nginx.enable = true;
    };

    scrapeConfigs = [
      (staticJob "nodes" ports.node-exporter [
        "desk.home"
        "htpc.home"
        "pi.home"
        "srv.wg"
      ])
      (staticJob "prometheus" ports.prometheus [ "127.0.0.1" ])
      (staticJob "nginx" ports.nginx-exporter [ "127.0.0.1" ])
      (staticJob "jellyfin" ports.jellyfin [ "127.0.0.1" ])
      (staticJob "wireguard" ports.wireguard [ "srv.wg" ])
    ];
  };

  services.nginx = {
    enable = true;
    statusPage = true;

    virtualHosts."prom.home" = {
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString ports.prometheus}";
      };
    };
  };
}
