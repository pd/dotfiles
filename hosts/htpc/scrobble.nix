{ config, ... }:
let
  storage.maloja = "/var/lib/maloja";
  storage.multiscrobbler = "/var/lib/multiscrobbler";
in
{
  sops.secrets."multiscrobbler.env" = { };
  sops.secrets."maloja-apikeys.yml" = { };
  sops.secrets."maloja-secrets.ini" = { };

  systemd.tmpfiles.settings."10-scrobble" = {
    "${storage.maloja}".d = {
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

  virtualisation.quadlet.containers = {
    maloja.containerConfig = {
      image = "docker.io/krateng/maloja:3.2.4@sha256:b5e62bbc0e69de2ee9ff60322cc144fe0263548ba1db1501fc657ff1b37a9904";
      environments = {
        # https://github.com/krateng/maloja/blob/master/settings.md
        MALOJA_DATA_DIRECTORY = "/data";
        MALOJA_SKIP_SETUP = "true";
      };
      volumes = [
        "${storage.maloja}:/data"
        "${config.sops.secrets."maloja-apikeys.yml".path}:/data/apikeys.yml:ro"
        "${config.sops.secrets."maloja-secrets.ini".path}:/run/secrets/maloja.ini"
      ];
      publishPorts = [ "127.0.0.1:42010:42010" ];
    };

    multiscrobbler.containerConfig = {
      image = "ghcr.io/foxxmd/multi-scrobbler:0.9.11@sha256:7212fa36e0bc25c88ab43cbe554d69361f331001654866ce048a3c4a8f93a333";
      volumes = [
        "${storage.multiscrobbler}:/config"
      ];
      environmentFiles = [ config.sops.secrets."multiscrobbler.env".path ];
      publishPorts = [ "127.0.0.1:9078:9078" ];
    };
  };
}
