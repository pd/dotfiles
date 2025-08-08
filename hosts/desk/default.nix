{
  config,
  lib,
  pkgs,
  ...
}:
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
  time.timeZone = "America/Chicago";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  # AMD GPU + dual output
  boot.initrd.kernelModules = [ "amdgpu" ];
  boot.kernelParams = [
    "video=DP-1:3840x2160"
    # "video=DP-2:1920x1200,rotate=90"
  ];

  # better support for very new AMDs in newish kernels
  boot.kernelPackages =
    if lib.versionOlder pkgs.linuxPackages.kernel.version pkgs.linuxPackages_6_15.kernel.version then
      pkgs.linuxPackages_6_15
    else
      builtins.warn "nixpkgs kernel version has surpassed the 6.15 in linuxPackages_6_15";

  # mostly functional audio
  security.polkit.enable = true;
  security.rtkit.enable = true;
  services.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # bluetooth
  services.blueman.enable = true;
  hardware.bluetooth = {
    enable = true;

    # https://github.com/bluez/bluez/blob/master/src/main.conf
    settings.General.Experimental = true; # show battery charge of devices
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

  # new gpu lets see how it goes
  programs.steam.enable = true;
}
