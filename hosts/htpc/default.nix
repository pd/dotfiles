{ ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/core
    ../../modules/nameserver
    ./caddy.nix
    ./prometheus.nix
    ./grafana.nix
    ./nas.nix
    ./tv.nix
  ];

  system.stateVersion = "24.05";

  networking.hostName = "htpc";
  lan.wifi.interface = "wlp0s20f3";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.enableRedistributableFirmware = true;
  security.rtkit.enable = true;

  time.timeZone = "America/Chicago";
}
