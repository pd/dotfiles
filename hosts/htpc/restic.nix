{
  config,
  pkgs,
  ...
}:
{
  age.secrets."restic.env" = { };
  age.secrets."restic-password" = { };

  services.restic.backups = {
    immich = {
      initialize = true;
      repository = "s3:s3.us-east-005.backblazeb2.com/pdbkupimg";
      environmentFile = config.age.secrets."restic.env".path;
      passwordFile = config.age.secrets."restic-password".path;

      paths = [
        "/media/photos"
        "/tmp/immich.pgdump"
      ];

      backupPrepareCommand = ''
        ${pkgs.sudo}/bin/sudo -u postgres ${config.services.postgresql.package}/bin/pg_dump \
          -Fc immich > /tmp/immich.pgdump
      '';
      backupCleanupCommand = "rm -f /tmp/immich.pgdump";

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

      createWrapper = true;
    };
  };
}
