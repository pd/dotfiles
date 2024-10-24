{
  lib,
  pkgs,
  config,
  ...
}:
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
  wan.natInterface = "ens3";

  time.timeZone = "America/Chicago";
}
