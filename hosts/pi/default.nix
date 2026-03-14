{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nameserver
  ];

  system.stateVersion = "24.05";
  time.timeZone = "America/Chicago";
  docs.enable = false;

  networking.hostName = "pi";
  lan.wired.interface = "end0";
  wg.natInterface = "end0";

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  environment.systemPackages = with pkgs; [
    nmap
    traceroute
  ];

  # check in with donix periodically
  heart.beat = true;

  monitoring.processes = {
    dnsmasq.comm = [ "dnsmasq" ];
  };
}
