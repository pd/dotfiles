{ config, ... }:
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
          url = "http://localhost:${toString config.services.prometheus.port}";
          isDefault = true;
        }
      ];
    };
  };

  services.nginx = {
    enable = true;

    virtualHosts."grafana.home" = {
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.grafana.settings.server.http_port}";
        recommendedProxySettings = true;
      };
    };
  };
}
