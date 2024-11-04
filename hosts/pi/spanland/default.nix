{ ... }:
{
  imports = [
    ./wg.nix
    ./echoserver.nix
  ];

  networking.search = [ "svc.cluster.local" ];
}
