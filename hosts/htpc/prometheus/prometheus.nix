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

    ruleFiles = [ ./prometheus.rules.yaml ];
  };

  services.caddy.virtualHosts."prom.home:80".extraConfig = ''
    reverse_proxy localhost:9999
  '';
}
