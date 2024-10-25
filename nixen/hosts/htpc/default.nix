{ pkgs, lib, ... }:
let
  transmission-done-script = pkgs.runCommand "install-script" { } ''
    install -Dm774 ${./transmission/torrent-completed.sh} $out/torrent-completed.sh
  '';
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

    transmission-done-script
  ];

  fileSystems."/nuc-bkup" = {
    fsType = "nfs";
    device = "nas.home:/volume1/nuc-bkup";
    options = [ "noatime" ];
  };

  fileSystems."/media" = {
    fsType = "nfs";
    device = "nas.home:/volume1/media";
    options = [ "noatime" ];
  };

  networking.firewall.allowedTCPPorts = [ 80 ];

  services.transmission = {
    enable = true;
    openPeerPorts = true;
    package = pkgs.transmission_4;
    performanceNetParameters = true;

    settings = {
      rpc-whitelist-enabled = true;
      rpc-whitelist = "127.0.0.*,192.168.*.*";

      rpc-host-whitelist-enabled = false;
      rpc-host-whitelist = "127.0.0.*,192.168.*.*";

      download-dir = "/media/transmission/done";
      incomplete-dir-enabled = true;
      incomplete-dir = "/media/transmission/wip";
      watch-dir-enabled = false;

      script-torrent-done-filename = "${transmission-done-script}/torrent-completed.sh";

      download-queue-enabled = false;

      # upnp is off so gotta pick something static
      peer-port = 52102;
      peer-limit-global = 500;
    };
  };

  services.nginx = {
    enable = true;

    virtualHosts."torrent.home" = {
      locations."/" = {
        proxyPass = "http://127.0.0.1:9091";
      };
    };
  };
}
