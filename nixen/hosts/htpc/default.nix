{ pkgs, lib, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/core
    ./prometheus.nix
    ./nas.nix
    ./tv.nix
  ];

  system.stateVersion = "24.05";

  networking.hostName = "htpc";
  lan.wifi.interface = "wlp0s20f3";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.enableRedistributableFirmware = true;
  hardware.pulseaudio.enable = true;
  security.rtkit.enable = true;

  time.timeZone = "America/Chicago";
}
