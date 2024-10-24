{
  lib,
  pkgs,
  config,
  ...
}:
let
  net = import ../../modules/net.nix;
in
{
  imports = [
    ../../modules/base.nix
    ../../modules/network
    ./dns.nix
    ./www.nix
    ./users/pd.nix
    ./users/rhys.nix
  ];

  system.stateVersion = "24.05";

  networking.hostName = "donix";

  lan.enable = false;
  wan.enable = true;
  wan.natInterface = "ens3";
  wan.ipv4 = net.hosts.donix.wg0.ip;
  wan.publicKey = net.hosts.donix.wg0.publicKey;

  time.timeZone = "America/Chicago";
}
