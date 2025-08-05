{ ... }:
{
  imports = [
    ./scrape-configs.nix
  ];

  services.prometheus = {
    enable = true;
    enableReload = true;
    webExternalUrl = "http://prom.home";
    port = 9999;

    extraFlags = [
      "--web.enable-admin-api"
    ];

    retentionTime = "60d";
    ruleFiles = [ ./prometheus.rules.yaml ];
  };
}
