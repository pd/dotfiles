{ config, lib, pkgs, ... }:
let
  pd = config.users.users.pd;

  nasIP = (import ../../modules/net.nix).hosts.nas.lan.ip;

  filebotd = pkgs.buildGoModule {
    name = "filebotd";
    src = ./filebotd;
    vendorHash = null;
    CGO_ENABLED = 0;
  };
in
{
  sops.secrets.filebot-license = {
    path = "${pd.home}/.local/share/filebot/data/.license";
    owner = pd.name;
    group = pd.group;
  };

  sops.secrets.jellyfin-api-key = {
    owner = pd.name;
    group = pd.group;
  };

  environment.systemPackages = with pkgs; [
    filebot
    filebotd
    nfs-utils
  ];

  systemd.services.filebotd = {
    enable = true;
    description = "filebotd";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    serviceConfig = {
      Type = "exec";
      User = pd.name;
      ExecStart = "${filebotd}/bin/filebotd";
      EnvironmentFile = config.sops.secrets.jellyfin-api-key.path;

      # TODO: lib.makeBinPath [pkgs.filebot] is insufficient,
      # cuz it then shells out to uname, dirname, etc. not sure
      # how to generate the "transitively required PATH" or
      # even what that's called in nixland
      Environment = "PATH=/run/current-system/sw/bin";
    };
  };

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

    virtualHosts."torrent.home" = {
      locations."/_hooks/" = {
        proxyPass = "http://127.0.0.1:12345";
      };

      locations."/" = {
        proxyPass = "http://nas.home:8080";
      };
    };
  };

}
