{ ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/base.nix
  ];

  system.stateVersion = "24.05";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "htpc";
  networking.networkmanager.enable = true;

  time.timeZone = "America/Chicago";
}
