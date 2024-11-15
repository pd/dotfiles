{ config, ... }:
{
  services.prometheus.exporters.node = {
    enable = true;
    openFirewall = true;
    listenAddress = if config.lan.enable then "0.0.0.0" else config.wg.ipv4;
  };
}
