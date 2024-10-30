{ lib, pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/core
    ./spanland
    ./dnsmasq.nix
  ];

  system.stateVersion = "24.05";

  networking.hostName = "pi";
  lan.wired.interface = "end0";

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
}
