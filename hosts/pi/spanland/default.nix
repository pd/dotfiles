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
}
