{ config, pkgs, ... }:
let
  storage.koito = "/var/lib/koito";
  storage.multiscrobbler = "/var/lib/multiscrobbler";
in
{
  sops.secrets."koito.env" = { };
  sops.secrets."multiscrobbler.env" = { };
  sops.secrets."npd.env" = { };

  systemd.tmpfiles.settings."10-scrobble" = {
    "${storage.koito}".d = {
      user = "root";
      group = "root";
      mode = "0755";
    };
    "${storage.multiscrobbler}".d = {
      user = "root";
      group = "root";
      mode = "0755";
    };
  };

  systemd.services.npd = {
    enable = true;
    description = "npd";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    environment = {
      ADDR = ":9776";
      JELLYFIN_URL = "http://jellyfin.home";
    };
    serviceConfig = {
      Type = "exec";
      User = config.users.users.pd.name;
      ExecStart = "${pkgs.pd.npd}/bin/npd";
      EnvironmentFile = config.sops.secrets."npd.env".path;
    };
  };

  virtualisation.quadlet.containers = {
    # koito uses host network to hit postgres. i don't want to
    # containerize postgres, and i'm too lazy to figure out the podman
    # incantations to make it reachable without host networking.
    koito.containerConfig = {
      image = "docker.io/gabehf/koito:v0.0.13@sha256:456beeeaa3485a52ce7073013e1479036da6a50ee49d07e9703815f194401dc8";
      networks = [ "host" ];
      environmentFiles = [ config.sops.secrets."koito.env".path ];
      environments = {
        KOITO_ALLOWED_HOSTS = "*";
        KOITO_BIND_ADDR = "127.0.0.1";
        KOITO_CONFIG_DIR = "/data";
        KOITO_DEFAULT_USERNAME = "pd";
        KOITO_ENABLE_FULL_IMAGE_CACHE = "true";
        KOITO_ENABLE_STRUCTURED_LOGGING = "true";
        KOITO_LOG_LEVEL = "info";
        KOITO_FETCH_IMAGES_DURING_IMPORT = "true";
        TZ = "America/Chicago";
      };
      volumes = [
        "${storage.koito}:/data"
      ];
    };

    multiscrobbler.containerConfig = {
      image = "ghcr.io/foxxmd/multi-scrobbler:0.9.11@sha256:7212fa36e0bc25c88ab43cbe554d69361f331001654866ce048a3c4a8f93a333";
      networks = [ "host" ];
      volumes = [
        "${storage.multiscrobbler}:/config"
      ];
      environments.TZ = "America/Chicago";
      environmentFiles = [ config.sops.secrets."multiscrobbler.env".path ];
    };
  };
}
