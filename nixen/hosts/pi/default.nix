{ lib, pkgs, ... } :
let
  net = import ../../modules/net.nix;
in
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/base.nix
    ../../modules/wg/client.nix
    ./dns.nix
  ];

  system.stateVersion = "24.05";

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  networking = {
    hostName = "pi";
    defaultGateway = {
      interface = "end0";
      address = net.nets.lan.gateway;
    };

    interfaces.end0.ipv4.addresses = [{
      address = net.hosts.pi.lan.ip;
      prefixLength = 24;
    }];

    nameservers = [
      "127.0.0.1"
      "1.1.1.1"
      "8.8.8.8"
    ];
  };
}
