{ config, pkgs, ... }:
{
  sops.secrets."immich.env" = {
    owner = config.services.immich.user;
    group = config.services.immich.group;
  };

  services.immich = {
    enable = true;
    package = pkgs.unstable.immich;
    secretsFile = config.sops.secrets."immich.env".path;
    database.createDB = true;
    mediaLocation = "/media/photos";
    accelerationDevices = null;
  };

  users.users.immich.extraGroups = [
    "video"
    "render"
  ];
}
