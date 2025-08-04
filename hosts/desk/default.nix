{ config, pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./disko.nix
    ./oci.nix
    ./wayland.nix
  ];

  system.stateVersion = "24.05";

  networking.hostName = "desk";

  lan.networkd = true;
  lan.wired.interface = "eth0";
  # lan.wifi.interface = "wlan0";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  time.timeZone = "America/Chicago";

  security.polkit.enable = true;
  security.rtkit.enable = true;
  services.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Needed for age-plugin-yubikey
  services.pcscd.enable = true;

  # desktop ends up occasionally wanting a password for
  # screenlocks, polkit agent, etc.
  sops.secrets.desktop-password.neededForUsers = true;
  users.users.pd.hashedPasswordFile = config.sops.secrets.desktop-password.path;

  # Reassign MACs so nixos is distinct from windows
  systemd.network.links."40-eth" = {
    matchConfig.PermanentMACAddress = "2c:f0:5d:db:8d:13";
    linkConfig = {
      MACAddressPolicy = "none";
      MACAddress = "2c:f0:5d:db:8d:f3";
      Name = "eth0";
    };
  };

  systemd.network.links."40-wlan" = {
    matchConfig.PermanentMACAddress = "14:cc:20:23:ea:6c";
    linkConfig = {
      MACAddressPolicy = "none";
      MACAddress = "14:cc:20:23:ea:fc";
      Name = "wlan0";
    };
  };

  # Installed at the system level for setcap + wireshark group
  programs.wireshark = {
    enable = true;
    package = pkgs.wireshark-qt;
  };
  users.users.pd.extraGroups = [ "wireshark" ];
}
