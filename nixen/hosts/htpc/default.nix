{ pkgs, config, ... }:
let
  net = import ../../modules/net.nix;
in
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/base.nix
  ];

  system.stateVersion = "24.05";

  sops.secrets.wifi = {
    mode = "0440";
    owner = "root";
    group = "wheel";
  };

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

  networking = {
    hostName = "htpc";

    nameservers = [
      net.hosts.pi.lan.ip
      "1.1.1.1"
      "2606:4700:4700::1111"
      "8.8.8.8"
    ];
    search = [ "home" ];

    # prefer networkmanager because at&t router won't let me
    # opt out of their bogus DNS responses
    wireless.enable = false;
    networkmanager = {
      enable = true;
      ensureProfiles.environmentFiles = [
        config.sops.secrets.wifi.path
      ];
      ensureProfiles.profiles = {
        wifi = {
          connection = {
            id = "$ssid";
            autoconnect = "yes";
            permissions = "";
            type = "wifi";
            interface-name = "wlp0s20f3";
          };
          ipv4 = {
            method = "auto";
            ignore-auto-dns = true;
          };
          ipv6 = {
            method = "auto";
            ignore-auto-dns = true;
          };
          wifi = {
            mode = "infrastructure";
            ssid = "$ssid";
          };
          wifi-security = {
            auth-alg = "open";
            key-mgmt = "wpa-psk";
            psk = "$psk";
          };
        };
      };
    };
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
