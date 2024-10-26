{ pkgs, lib, ... }:
let
  nasIP = (import ../../modules/net.nix).hosts.nas.lan.ip;
in
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/core
    ./transmission
    ./tv.nix
  ];

  system.stateVersion = "24.05";

  networking.hostName = "htpc";
  lan.wifi.interface = "wlp0s20f3";
  networking.firewall.allowedTCPPorts = [ 80 ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.enableRedistributableFirmware = true;
  hardware.pulseaudio.enable = true;
  security.rtkit.enable = true;

  time.timeZone = "America/Chicago";

  environment.systemPackages = with pkgs; [
    nfs-utils
  ];

  # TODO: figure out why the ordering of the automount
  # means dns resolution isn't functioning yet
  fileSystems."/nuc-bkup" = {
    fsType = "nfs";
    device = "${nasIP}:/volume1/nuc-bkup";
    options = [ "noatime" "x-systemd.automount" ];
  };

  fileSystems."/media" = {
    fsType = "nfs";
    device = "${nasIP}:/volume1/media";
    options = [ "noatime" "x-systemd.automount" ];
  };

}
