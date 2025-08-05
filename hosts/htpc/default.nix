{ ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nameserver
    ./prometheus
    ./httpd.nix
    ./grafana.nix
    ./jellyfin.nix
    ./nas.nix
  ];

  system.stateVersion = "24.05";

  networking.hostName = "htpc";
  lan.wifi.interface = "wlp0s20f3"; # TODO just run cable, networkd+wifi sucks
  # lan.networkd = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.enableRedistributableFirmware = true;
  security.rtkit.enable = true;

  time.timeZone = "America/Chicago";

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

}
