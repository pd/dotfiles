{
  config,
  inputs,
  pd,
  pkgs,
  ...
}:
let
  user = config.users.users.pd;
in
{
  imports = [
    "${inputs.private}/archival"
  ];

  sops.secrets.filebot-license = {
    path = "${user.home}/.local/share/filebot/data/.license";
    owner = user.name;
    group = user.group;
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
      qbt-hooks
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

  fileSystems."/mnt/qbittorrent" = {
    fsType = "nfs";
    device = "nas.home:/volume1/state/qbittorrent";
    options = [
      "noatime"
      "x-systemd.automount"
      "x-systemd.requires=network-online.target"
    ];
  };

  sops.secrets."qbt-categories" = {
    owner = config.users.users.qbittorrent.name;
    group = config.users.users.qbittorrent.group;
  };
  services.qbittorrent = {
    enable = true;
    profileDir = "/mnt/qbittorrent";
    openFirewall = true;
    webuiPort = 8000;
    torrentingPort = 51000;
    serverConfig = {
      LegalNotice.Accepted = true;
      Application.FileLogger.Enabled = false;

      Preferences = {
        General = {
          Locale = "en";
          StatusbarExternalIPDisplayed = true;
        };

        WebUI = {
          Password_PBKDF2 = "@ByteArray(pIRT6QqiBA+oRI6yl/hUAA==:m1NTlNoEszq/0rVAiGq+UE74hyie0kbL70k3cl4baWxaRIlWT2usr41fTrutCkRAoy7JUorUVzyEFja6M1O75Q==)";
          AuthSubnetWhitelistEnabled = true;
          AuthSubnetWhitelist = pd.net.lan.cidr;
          LocalHostAuth = false;
        };
      };

      BitTorrent.Session = {
        # everything's private
        DHTEnabled = false;
        PeXEnabled = false;
        LSDEnabled = false;
        Encryption = 0; # 0=prefer, 1=require, 2=disable

        # announces fail if i'm not explicit about interface, dunno
        Interface = config.lan.wired;
        InterfaceName = config.lan.wired;

        # seed everything at all times
        QueueingSystemEnabled = false;

        # reasonable defaults, categories control final resting place
        DefaultSavePath = "/media/torrents/done/uncategorized/";
        TempPathEnabled = true;
        TempPath = "/media/torrents/wip/";
        TorrentExportDirectory = "/media/torrents/dots/";

        # shuffle files around for us as categories get updated
        DisableAutoTMMByDefault = false;
      };

      Core = {
        AutoDeleteAddedTorrentFile = "Never";
      };

      # %N: Torrent name
      # %L: Category
      # %G: Tags (separated by comma)
      # %F: Content path (same as root path for multifile torrent)
      # %R: Root path (first torrent subdirectory path)
      # %D: Save path
      # %C: Number of files
      # %Z: Torrent size (bytes)
      # %T: Current tracker
      # %I: Info hash v1
      # %J: Info hash v2
      # %K: Torrent ID
      AutoRun = {
        OnTorrentAdded.Enabled = true;
        OnTorrentAdded.Program = ''${pkgs.pd.qbt-hooks}/bin/qbt-on-add \"%I\" \"%T\" \"%L\"'';
        enabled = true;
        program = ''${pkgs.pd.qbt-hooks}/bin/qbt-on-complete \"%F\" \"%L\"'';
      };
    };
  };

  sops.secrets."qbittorrent-exporter.env" = { };
  networking.firewall.allowedTCPPorts = [ 8090 ];
  systemd.services.qbittorrent-exporter = {
    enable = true;
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    serviceConfig = {
      Type = "exec";
      Restart = "on-failure";
      DynamicUser = true;
      EnvironmentFile = config.sops.secrets."qbittorrent-exporter.env".path;
      ExecStart = "${pkgs.prometheus-qbittorrent-exporter}/bin/qbit-exp -e";
    };
    environment = {
      QBITTORRENT_BASE_URL = "http://torrent.home";
      QBITTORRENT_USERNAME = "admin";
      ENABLE_HIGH_CARDINALITY = "true";
    };
  };

  sops.secrets."filebotd.env" = { };
  systemd.services.filebotd = {
    enable = true;
    description = "filebotd";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    path = [
      # TODO: PATH should be handled by pkgs at this point
      pkgs.pd.mediaman
      pkgs.filebot
    ];
    serviceConfig = {
      Type = "exec";
      User = user.name;
      ExecStart = "${pkgs.pd.filebotd}/bin/filebotd";
      EnvironmentFile = config.sops.secrets."filebotd.env".path;
    };
  };

  systemd.services.media-prune = {
    serviceConfig = {
      Type = "oneshot";
      User = user.name;
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
