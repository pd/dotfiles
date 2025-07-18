{ ... }:
{
  networking.firewall.allowedTCPPorts = [
    80
    2020
  ];

  services.caddy = {
    enable = true;

    globalConfig = ''
      metrics { per_host }
    '';

    virtualHosts.":2020".extraConfig = ''
      metrics
    '';
  };
}
