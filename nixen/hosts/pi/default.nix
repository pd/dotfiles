{ lib, pkgs, ... } :
let
  net = import ../../modules/net.nix;
  wg0 = net.hosts.pi.wg0;
in
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/base.nix
    ../../modules/network
    ./dns.nix
  ];

  system.stateVersion = "24.05";

  networking.hostName = "pi";
  lan.ipv4 = net.hosts.pi.lan.ip;
  lan.wired.interface = "end0";

  wan.enable = true;
  wan.ipv4 = wg0.ip;
  wan.publicKey = wg0.publicKey;

  networking.nameservers = lib.mkForce [
    "127.0.0.1"
    "1.1.1.1"
    "8.8.8.8"
  ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
}
