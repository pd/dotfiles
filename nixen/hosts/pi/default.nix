{ lib, pkgs, ... } :
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/base.nix
  ];

  system.stateVersion = "24.05";

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "pi";
  networking.interfaces.end0.ipv4.addresses = [{
    address = "192.168.1.13";
    prefixLength = 24;
  }];
  networking.defaultGateway = {
    address = "192.168.1.254";
    interface = "end0";
  };
  networking.nameservers = [
    "1.1.1.1"
    "8.8.8.8"
  ];
}
