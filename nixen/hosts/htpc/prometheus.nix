{ config, ... }:
let
  inherit (builtins) map toString;
  targets = port: names: map (n: "${n}:${toString port}") names;
  exporters = config.services.prometheus.exporters;
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
      {
        job_name = "nodes";
        scrape_interval = "60s";
        static_configs = [
          {
            targets = targets 9100 [
              "desk.home"
              "htpc.home"
              "pi.home"
              "10.100.100.1" # TODO donix has no internal name yet
            ];
          }
        ];
      }
      {
        job_name = "prometheus";
        scrape_interval = "60s";
        static_configs = [ { targets = targets config.services.prometheus.port [ "127.0.0.1" ]; } ];
      }
      {
        job_name = "nginx";
        scrape_interval = "60s";
        static_configs = [ { targets = targets exporters.nginx.port [ "127.0.0.1" ]; } ];
      }
      {
        job_name = "jellyfin";
        scrape_interval = "60s";
        static_configs = [ { targets = targets 8096 [ "127.0.0.1" ]; } ];
      }
    ];
  };

  services.nginx = {
    enable = true;
    statusPage = true;

    virtualHosts."prom.home" = {
      locations."/" = {
        proxyPass = "http://127.0.0.1:9999";
      };
    };
  };
}
