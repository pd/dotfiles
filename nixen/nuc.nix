{...} :
{
  imports = [
    ./modules/base.nix
  ];

  system.stateVersion = "24.05";

  networking.hostName = "nuc";
  networking.firewall.allowedTCPPorts = [53 80];
  networking.firewall.allowedUDPPorts = [53];

  time.timeZone = "America/Chicago";
}
