{ config, ... }:
{
  services.grafana = {
    enable = true;
    settings = {
      server = {
        http_addr = "127.0.0.1";
      };

      # instance holds no DB secrets (anonymous admin, passwordless datasource),
      # so the pre-26.05 default key protects nothing and needs no rotation
      security.secret_key = "SW2YcwTIb9zpOOhoPsMm";

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
}
