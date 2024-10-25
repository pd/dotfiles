{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/core
  ];

  system.stateVersion = "24.05";

  networking.hostName = "htpc";
  lan.wifi.interface = "wlp0s20f3";

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

  networking.firewall.allowedTCPPorts = [ 80 ];

  services.nginx = {
    enable = true;
    virtualHosts."torrent.home".locations."/" = {
      proxyPass = "http://nas.home:8080";
    };
  };
}
