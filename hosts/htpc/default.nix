{ ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nameserver
    ./prometheus
    ./caddy.nix
    ./grafana.nix
    ./jellyfin.nix
    ./nas.nix
  ];

  system.stateVersion = "24.05";

  networking.hostName = "htpc";
  lan.wifi.interface = "wlp0s20f3";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.enableRedistributableFirmware = true;
  security.rtkit.enable = true;

  services.keyd = {
    enable = true;
    keyboards = {
      default = {
        ids = [ "*" ];
        settings = {
          main = {
            capslock = "layer(control)";
          };
        };
      };
    };
  };

  time.timeZone = "America/Chicago";
}
