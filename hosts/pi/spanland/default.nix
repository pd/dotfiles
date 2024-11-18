{ ... }:
{
  imports = [
    ./wg.nix
    ./echoserver.nix
  ];

  networking.search = [ "svc.cluster.local" ];

  # deadc0decafe
  networking.firewall.allowedTCPPorts = [ 49374 ];
  networking.firewall.allowedUDPPorts = [ 49374 ];

  # pi can also resolve the spanland network
  services.dnsmasq.settings.server = [
    "/svc.cluster.local/172.30.30.1"
  ];
}
