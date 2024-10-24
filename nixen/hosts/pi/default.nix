{ lib, pkgs, ... } :
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/base.nix
    ../../modules/network
    ./dns.nix
  ];

  system.stateVersion = "24.05";

  networking.hostName = "pi";
  lan.wired.interface = "end0";

  networking.nameservers = lib.mkForce [
    "127.0.0.1"
    "1.1.1.1"
    "8.8.8.8"
  ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
}
