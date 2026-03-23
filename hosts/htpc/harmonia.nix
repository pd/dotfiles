{ config, pkgs, ... }:
{
  sops.secrets.harmonia-key = { };
  services.harmonia = {
    enable = true;
    package = pkgs.unstable.harmonia;
    signKeyPaths = [ config.sops.secrets.harmonia-key.path ];
  };
}
