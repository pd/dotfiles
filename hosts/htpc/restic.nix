{
  config,
  lib,
  pkgs,
  ...
}:
let
  dumpDir = "/tmp/dbs";

  dumpDB =
    {
      postgres ? { },
      sqlite ? { },
    }:
    let
      sudo = lib.getExe' pkgs.sudo "sudo";
      pg_dump = lib.getExe' config.services.postgresql.package "pg_dump";
      sqlite3 = lib.getExe' pkgs.sqlite "sqlite3";

      dump = {
        pg = name: db: "${sudo} -u postgres ${pg_dump} -Fc ${db} > ${dumpDir}/${name}.pgdump";
        sqlite = name: db: "${sqlite3} file:${db}?mode=rw '.backup ${dumpDir}/${name}.db'";
      };
    in
    ''
      set -eu
      mkdir -p ${dumpDir}
      ${lib.concatLines (lib.mapAttrsToList dump.pg postgres)}
      ${lib.concatLines (lib.mapAttrsToList dump.sqlite sqlite)}
    '';

  backup =
    {
      repository,
      secrets,
      dirs ? [ ],
      dbs ? { },
    }:
    {
      inherit repository;
      environmentFile = config.age.secrets.${secrets}.path;
      passwordFile = config.age.secrets."restic-password".path;
      initialize = true;
      createWrapper = true;

      timerConfig = {
        OnCalendar = "daily";
        Persistent = true;
        RandomizedDelaySec = "1h";
      };

      pruneOpts = [
        "--keep-daily 7"
        "--keep-weekly 4"
        "--keep-monthly 12"
      ];

      paths = dirs ++ lib.optional (dbs != { }) dumpDir;
      backupPrepareCommand = if dbs != { } then dumpDB dbs else null;
    };
in
{
  age.secrets."restic-immich.env" = { };
  age.secrets."restic-apps.env" = { };
  age.secrets."restic-password" = { };

  services.restic.backups = {
    immich = backup {
      repository = "s3:s3.us-east-005.backblazeb2.com/pdbkupimg";
      secrets = "restic-immich.env";
      dirs = [ "/media/photos" ];
      dbs.postgres.immich = "immich";
    };

    apps = backup {
      repository = "s3:s3.us-east-005.backblazeb2.com/pdbkupapps";
      secrets = "restic-apps.env";
      dirs = [
        "/var/lib/jellyfin/config"
        "/var/lib/lidarr/.config/Lidarr/config.xml"
        "/var/lib/private/prowlarr/config.xml"
        "/var/lib/radarr/.config/Radarr/config.xml"
        "/var/lib/seerr/config/settings.json"
        "/var/lib/sonarr/.config/NzbDrone/config.xml"
      ];
      dbs = {
        postgres.koito = "koito";
        sqlite = {
          grafana = "/var/lib/grafana/data/grafana.db";
          jellyfin = "/var/lib/jellyfin/data/jellyfin.db";
          lidarr = "/var/lib/lidarr/.config/Lidarr/lidarr.db";
          prowlarr = "/var/lib/private/prowlarr/prowlarr.db";
          radarr = "/var/lib/radarr/.config/Radarr/radarr.db";
          seerr = "/var/lib/seerr/config/db/db.sqlite3";
          sonarr = "/var/lib/sonarr/.config/NzbDrone/sonarr.db";
        };
      };
    };
  };

  # make sure /tmp gets wiped after each run
  systemd.services = lib.mapAttrs' (
    name: _: lib.nameValuePair "restic-backups-${name}" { serviceConfig.PrivateTmp = true; }
  ) config.services.restic.backups;
}
