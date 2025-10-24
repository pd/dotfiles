{
  config,
  inputs,
  pkgs,
  ...
}:
let
  pd = config.users.users.pd;
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

  sops.secrets."rtorrent-exporter.yaml" = {
    owner = config.users.users.prometheus.name;
  };

  environment.systemPackages =
    with pkgs;
    [
      chromaprint # audio id for filebot
      filebot
      nfs-utils
    ]
    ++ (with pkgs.pd; [
      filebotd
      mediaman
      rtorrent-exporter
      xtor
    ]);

  fileSystems."/media" = {
    fsType = "nfs";
    device = "nas.home:/volume1/media";
    options = [
      "noatime"
      "x-systemd.automount"
      "x-systemd.requires=network-online.target"
    ];
  };

  sops.secrets."filebotd.env" = { };
  systemd.services.filebotd = {
    enable = true;
    description = "filebotd";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    path = [
      # TODO: PATH should be handled by pkgs at this point
      pkgs.pd.mediaman
      pkgs.filebot
    ];
    serviceConfig = {
      Type = "exec";
      User = pd.name;
      ExecStart = "${pkgs.pd.filebotd}/bin/filebotd";
      EnvironmentFile = config.sops.secrets."filebotd.env".path;
    };
  };

  networking.firewall.allowedTCPPorts = [ 9135 ];
  systemd.services.rtorrent-exporter = {
    enable = true;
    description = "rtorrent-exporter";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    serviceConfig = {
      Type = "exec";
      Restart = "on-failure";
      User = config.users.users.prometheus.name;
      ExecStart = ''
        ${pkgs.pd.rtorrent-exporter}/bin/rtorrent-exporter \
          --config ${config.sops.secrets."rtorrent-exporter.yaml".path}
      '';
    };
  };

  systemd.services.media-prune = {
    serviceConfig = {
      Type = "oneshot";
      User = "pd";
    };

    script = "${pkgs.pd.mediaman}/bin/media-prune";
  };

  systemd.timers.media-prune = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = [ "60m" ];
      OnUnitActiveSec = [ "720m" ];
    };
  };
}
