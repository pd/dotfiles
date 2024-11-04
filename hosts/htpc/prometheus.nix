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
      rtorrent = 9135;
      snmp = 9116;
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
      (staticJob "jellyfin" ports.jellyfin [ "127.0.0.1" ])
      (staticJob "nginx" ports.nginx-exporter [ "127.0.0.1" ])
      (staticJob "nodes" ports.node-exporter [
        "desk.home"
        "htpc.home"
        "pi.home"
        "nas.home"
        "srv.wg"
      ])
      (staticJob "prometheus" ports.prometheus [ "127.0.0.1" ])
      (staticJob "rtorrent" ports.rtorrent [ "127.0.0.1" ])
      (staticJob "snmp-exporter" ports.snmp [ "nas.home" ])
      (
        (staticJob "snmp" ports.snmp [ "nas.home" ])
        // {
          metrics_path = "/snmp";
          params = {
            auth = [ "public_v2" ];
            module = [
              "if_mib"
              "synology"
            ];
            target = [ "tcp://nas.home" ];
          };
        }
      )
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
