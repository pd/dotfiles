{ config, ... }:
let
  ports = {
    grafana = toString config.services.grafana.settings.server.http_port;
    prometheus = toString config.services.prometheus.port;
  };
in
{
  services.grafana = {
    enable = true;
    settings = {
      server = {
        http_addr = "127.0.0.1";
      };

      "auth.anonymous".enabled = true;
      "auth.anonymous".org_name = "pd";
      "auth.anonymous".org_role = "Admin";
    };

    provision = {
      enable = true;
      datasources.settings.datasources = [
        {
          name = "prom";
          type = "prometheus";
          access = "proxy";
          url = "http://localhost:${ports.prometheus}";
          isDefault = true;
        }
      ];
    };
  };

  services.caddy.virtualHosts."grafana.home:80".extraConfig = ''
    reverse_proxy localhost:${ports.grafana}
  '';
}
