{ config, ... }:
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
        job_name = "prometheus";
        scrape_interval = "60s";
        static_configs = [{
          targets = [ "127.0.0.1:${builtins.toString config.services.prometheus.port}" ];
        }];
      } {
        job_name = "nodes";
        scrape_interval = "60s";
        static_configs = [{
          targets = builtins.map (n: "${n}:9100") [
            "desk.home"
            "htpc.home"
            "pi.home"

            "10.100.100.1" # TODO donix has no internal name yet?
          ];
        }];
      } {
        job_name = "nginx";
        scrape_interval = "60s";
        static_configs = [{
          targets = [ "127.0.0.1:${builtins.toString config.services.prometheus.exporters.nginx.port}" ];
        }];
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
