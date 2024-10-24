{ pkgs, config, ... }:
let
  net = import ../../modules/net.nix;
in
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/base.nix
    ../../modules/network
  ];

  system.stateVersion = "24.05";

  networking.hostName = "htpc";
  lan.wifi.interface = "wlp0s20f3";

  wan.enable = true;
  wan.ipv4 = net.hosts.htpc.wg0.ip;
  wan.publicKey = net.hosts.htpc.wg0.publicKey;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.enableRedistributableFirmware = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  time.timeZone = "America/Chicago";

  services.jellyfin = {
    enable = true;
    openFirewall = true;
  };

  environment.systemPackages = with pkgs; [
    jellyfin
    jellyfin-web
    jellyfin-ffmpeg
    nfs-utils
  ];

  fileSystems."/media" = {
    fsType = "nfs";
    device = "nas.home:/volume1/nuc-bkup";
    options = [ "noatime" ];
  };

  # rtorrent on nas ideally
  # then nginx + https://github.com/jesec/flood
}
