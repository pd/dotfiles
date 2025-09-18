{ config, lib, ... }:
with lib;
{
  options = {
    docs.enable = mkOption {
      type = types.bool;
      default = true;
      description = "install docs";
    };
  };

  config = {
    documentation = mkIf (!config.docs.enable) {
      enable = false;
      doc.enable = false;
      info.enable = false;
      man.enable = false;
      nixos.enable = false;
    };
  };
}
