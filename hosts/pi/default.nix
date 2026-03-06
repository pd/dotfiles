{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nameserver
  ];

  system.stateVersion = "24.05";

  networking.hostName = "pi";
  lan.wired.interface = "end0";
  wg.natInterface = "end0";

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  # check in with donix periodically
  heart.beat = true;

  environment.systemPackages = with pkgs; [
    nmap
    traceroute
  ];

  docs.enable = false;
}
