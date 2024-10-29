{ config, pkgs, ... }:
{
  services.jellyfin = {
    enable = true;
    openFirewall = true;
  };

  services.xserver = {
    enable = true;

    desktopManager.kodi = {
      enable = true;
      package = (pkgs.kodi.withPackages (kodiPackages: [ kodiPackages.jellyfin ]));
    };

    displayManager.lightdm.greeter.enable = false;
    displayManager.setupCommands = ''
      /run/current-system/sw/bin/xset -dpms
      /run/current-system/sw/bin/xset s off
    '';
  };

  services.displayManager.autoLogin = {
    enable = true;
    user = config.users.users.kodi.name;
  };

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-compute-runtime
      intel-media-driver
      intel-media-sdk
      intel-vaapi-driver
      vaapiVdpau
    ];
  };

  users.users.kodi = {
    isNormalUser = true;

    # TODO seems like this ought to be required yet ...
    # extraGroups = ["audio" "transmission" "video"];
  };

  environment.systemPackages = with pkgs; [
    jellyfin
    jellyfin-web
    jellyfin-ffmpeg
  ];

  services.nginx = {
    enable = true;

    virtualHosts."kodi.home".locations."/" = {
      proxyPass = "http://127.0.0.1:8080";
    };

    virtualHosts."jellyfin.home".locations."/" = {
      proxyPass = "http://127.0.0.1:8096";
    };
  };

  # TODO: deal with ipv6 or "private dns" for android
  # my phone uses the pi for dns, but injects an ipv6 dns
  # resolver ahead of it no matter what i do. so i need
  # direct access to kodi to use a remote.
  networking.firewall.allowedTCPPorts = [ 8080 ];
}
