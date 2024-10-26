{ pkgs, ... }:
let
  transmission-scripts = pkgs.runCommand "install" { } ''
    install -Dm774 ${./torrent-completed.sh} $out/torrent-completed.sh
  '';
in
{
  environment.systemPackages = with pkgs; [
    transmission-scripts
  ];

  services.transmission = {
    enable = true;
    openPeerPorts = true;
    package = pkgs.transmission_4;
    performanceNetParameters = true;

    settings = {
      rpc-whitelist-enabled = true;
      rpc-whitelist = "127.0.0.*,192.168.*.*";

      rpc-host-whitelist-enabled = false;
      rpc-host-whitelist = "127.0.0.*,192.168.*.*";

      download-dir = "/media/transmission/done";
      incomplete-dir-enabled = true;
      incomplete-dir = "/media/transmission/wip";
      watch-dir-enabled = false;

      script-torrent-done-filename = "${transmission-scripts}/torrent-completed.sh";

      download-queue-enabled = false;

      # upnp is off so gotta pick something static
      peer-port = 52102;
      peer-limit-global = 500;
    };
  };

  services.nginx = {
    enable = true;

    virtualHosts."torrent.home" = {
      locations."/" = {
        proxyPass = "http://127.0.0.1:9091";
      };
    };
  };
}
