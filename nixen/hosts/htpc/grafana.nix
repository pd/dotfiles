{ config, ... }:
{
  services.grafana = {
    enable = true;
    settings = {
      server = {
        http_addr = "127.0.0.1";
      };
    };
  };

  services.nginx = {
    enable = true;

    virtualHosts."grafana.home" = {
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.grafana.settings.server.http_port}";
      };
    };
  };
}
