{ config, pkgs, ... }:
{
  age.secrets.harmonia-key = { };
  services.harmonia = {
    package = pkgs.unstable.harmonia;
    cache = {
      enable = true;
      signKeyPaths = [ config.age.secrets.harmonia-key.path ];
    };
  };
}
