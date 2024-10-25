{ pkgs, lib, ... }:
let
  directorrent = pkgs.buildGoModule {
    name = "directorrent";
    src = ./directorrent;
    vendorHash = null;
    CGO_ENABLED = 0;
    meta = {
      description = "Custom HTTP server to do some torrent juggling";
    };
  };
in
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

    directorrent
  ];

  fileSystems."/media" = {
    fsType = "nfs";
    device = "nas.home:/volume1/nuc-bkup";
    options = [ "noatime" ];
  };

  networking.firewall.allowedTCPPorts = [ 80 ];

  services.nginx = {
    enable = true;
    virtualHosts."torrent.home" = {
      locations."~^/directorrent/(.*)$" = {
        proxyPass = "http://127.0.0.1:12345/$1$is_args$args";
      };

      locations."/" = {
        proxyPass = "http://nas.home:8080";
      };
    };
  };

  systemd.units."directorrent.service" = {
    enable = true;
    wantedBy = ["multi-user.target"];
    text = ''
      [Unit]
      Description=directorrent

      [Service]
      ExecStart=${directorrent}/bin/directorrent
      Type=exec
      Restart=always
      KillMode=process
    '';
  };
}
