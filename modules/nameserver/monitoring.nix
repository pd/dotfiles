{ config, ... }:
{
  services.prometheus.exporters.dnsmasq.enable = true;
  networking.firewall.allowedTCPPorts = [
    config.services.prometheus.exporters.dnsmasq.port
  ];
}
