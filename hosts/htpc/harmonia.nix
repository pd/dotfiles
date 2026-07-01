{ config, pkgs, ... }:
{
  sops.secrets.harmonia-key = { };
  services.harmonia = {
    package = pkgs.unstable.harmonia;
    cache = {
      enable = true;
      signKeyPaths = [ config.sops.secrets.harmonia-key.path ];
    };
  };
}
