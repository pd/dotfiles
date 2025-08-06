{ config, pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./disko.nix
    ./network.nix
    ./oci.nix
    ./wayland.nix
  ];

  system.stateVersion = "24.05";
  networking.hostName = "desk";

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

  # Installed at the system level for setcap + wireshark group
  programs.wireshark = {
    enable = true;
    package = pkgs.wireshark-qt;
  };
  users.users.pd.extraGroups = [ "wireshark" ];
}
