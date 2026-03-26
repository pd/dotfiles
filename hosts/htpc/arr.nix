{ ... }:
{
  services.prowlarr.enable = true;
  services.radarr.enable = true;
  services.sonarr.enable = true;
  services.lidarr.enable = true;

  # seerr preview image with music/lidarr support. almost certainly
  # a bad idea to run this but here i am doing it anyway
  #
  # tracking:
  # https://github.com/seerr-team/seerr/discussions/2160
  # https://github.com/seerr-team/seerr/pull/2132
  # https://github.com/NixOS/nixpkgs/pull/500782
  systemd.tmpfiles.settings."10-seerr" = {
    "/var/lib/seerr/config".d = {
      user = "1000";
      group = "1000";
      mode = "0755";
    };
  };

  virtualisation.quadlet.containers.seerr.containerConfig = {
    image = "ghcr.io/seerr-team/seerr:preview-music-support@sha256:f829abfe6a127a58c1e38e63a79680edf56d09607686aa4b2a2ef75ae1883bcf";
    networks = [ "host" ];
    environments.TZ = "America/Chicago";
    volumes = [
      "/var/lib/seerr/config:/app/config"
    ];
  };
}
