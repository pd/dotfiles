{
  lib,
  pkgs,
  config,
  ...
}:
{
  imports = [
    ../../modules/base.nix
    ../../modules/wg/server.nix
    ./dns.nix
    ./www.nix
    ./users/pd.nix
    ./users/rhys.nix
  ];

  system.stateVersion = "24.05";
  networking.hostName = "donix";
  time.timeZone = "America/Chicago";
}
