{
  config,
  inputs,
  pkgs,
  ...
}:
let
  pd = config.users.users.pd;

  filebotd = pkgs.buildGoModule {
    name = "filebotd";
    src = ./filebotd;
    vendorHash = null;
    env.CGO_ENABLED = 0;
  };

  media-prune = pkgs.writeShellScript "media-prune" (builtins.readFile ./media-prune.sh);
  media-sort = pkgs.writeShellScriptBin "media-sort" (builtins.readFile ./media-sort.sh);

  rtorrent-exporter = pkgs.stdenv.mkDerivation {
    name = "rtorrent-exporter";
    src = pkgs.fetchurl {
      url = "https://github.com/thde/rtorrent_exporter/releases/download/v1.3.2/rtorrent_exporter_1.3.2_linux_amd64.tar.gz";
      sha256 = "24psJpXnKniTHg3BqXoVyazwBHeb75r7F7/LX3tC0SY=";
    };
    phases = [
      "unpackPhase"
      "installPhase"
    ];
    sourceRoot = ".";
    installPhase = ''
      mkdir -p $out/bin
      cp rtorrent_exporter $out/bin
    '';
  };
in
{
  imports = [
    "${inputs.private}/archival"
  ];

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
    media-sort
    nfs-utils
    rtorrent-exporter
  ];

  fileSystems."/media" = {
    fsType = "nfs";
    device = "nas.home:/volume1/media";
    options = [
      "noatime"
      "x-systemd.automount"
      "x-systemd.requires=network-online.target"
    ];
  };

  systemd.services.filebotd = {
    enable = true;
    description = "filebotd";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    path = [ pkgs.filebot ];
    serviceConfig = {
      Type = "exec";
      User = pd.name;
      ExecStart = "${filebotd}/bin/filebotd";
      EnvironmentFile = config.sops.secrets.jellyfin-api-key.path;
    };
  };

  systemd.services.rtorrent-exporter = {
    enable = true;
    description = "rtorrent-exporter";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    serviceConfig = {
      Type = "exec";
      User = config.users.users.prometheus.name;
      ExecStart = ''
        ${rtorrent-exporter}/bin/rtorrent_exporter \
          --rtorrent.scrape-uri="http://nas.home:8000/RPC2"
      '';
    };
  };

  systemd.services.media-prune = {
    serviceConfig = {
      Type = "oneshot";
      User = "pd";
    };

    script = "${media-prune}";
  };

  systemd.timers.media-prune = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = [ "60m" ];
      OnUnitActiveSec = [ "720m" ];
    };
  };

  networking.firewall.allowedTCPPorts = [ 80 ];

  services.caddy.virtualHosts = {
    "store.home:80".extraConfig = ''
      reverse_proxy nas.home:5000
    '';

    "torrent.home:80".extraConfig = ''
      handle /_hooks/* {
        reverse_proxy localhost:12345
      }

      handle {
        reverse_proxy nas.home:8080
      }
    '';
  };
}
