{ pkgs, lib, ... }:
let
  nasIP = (import ../../modules/net.nix).hosts.nas.lan.ip;
in
{
  environment.systemPackages = with pkgs; [
    nfs-utils
  ];

  # TODO: figure out why the ordering of the automount
  # means dns resolution isn't functioning yet
  fileSystems."/media" = {
    fsType = "nfs";
    device = "${nasIP}:/volume1/media";
    options = [ "noatime" "x-systemd.automount" ];
  };

  networking.firewall.allowedTCPPorts = [ 80 ];

  services.nginx = {
    enable = true;

    virtualHosts."store.home".locations."/" = {
      proxyPass = "http://nas.home:5000";
    };

    virtualHosts."torrent.home".locations."/" = {
      proxyPass = "http://nas.home:8080";
    };
  };

}
