{
  lib,
  pkgs,
  config,
  ...
} : {
  imports = [
    ./hardware-configuration.nix
    ../../modules/base.nix
  ];

  system.stateVersion = "24.05";

  sops.defaultSopsFile = ./secrets.yaml;
  sops.secrets.wireguard-private-key = {
    mode = "0440";
    owner = config.users.users.root.name;
    group = "wheel";
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = ["ntfs"];

  networking.hostName = "desk";
  networking.networkmanager.enable = true;
  networking.firewall = {
    allowedTCPPorts = [ 22 ];
    allowedUDPPorts = [ 51820 ];
  };

  networking.wireguard.interfaces = {
    wg0 = {
      listenPort = 51820;
      ips = ["10.100.100.10/32"];
      privateKeyFile = config.sops.secrets.wireguard-private-key.path;

      peers = [
        {
          endpoint = "donix.krh.me:51820";
          publicKey = "WZgf+DC6SBQeatqOgpC2j6tvIu5VxKi/WgdbIU/m7wg=";
          allowedIPs = [ "10.100.100.0/24" ];
          persistentKeepalive = 25;
        }
      ];
    };
  };

  time.timeZone = "America/Chicago";

  # lol, for now
  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };
  services.displayManager.autoLogin = {
    enable = true;
    user = "pd";
  };
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@tty1".enable = false;

  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # TODO: can i turn this back on?
  # things want a password to log in after screenlock etc
  users.mutableUsers = lib.mkForce true;

  environment.systemPackages = with pkgs; [
    age
    sops
  ];
}
