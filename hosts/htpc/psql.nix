{
  config,
  lib,
  pkgs,
  ...
}:
{
  sops.secrets."psql-init.sql" = {
    owner = "postgres";
  };

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_17_jit;

    ensureDatabases = [ "koito" ];
    ensureUsers = [
      {
        name = "koito";
        ensureDBOwnership = true;
        ensureClauses.login = true;
      }
    ];
  };

  systemd.services.postgresql.postStart = lib.mkAfter ''
    psql < ${config.sops.secrets."psql-init.sql".path}
  '';
}
