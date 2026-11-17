{ config, lib, ... }:
{
  options.age.secrets = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule (
        { name, ... }:
        {
          config.file = lib.mkDefault ../../../hosts/${config.networking.hostName}/secrets/${name}.age;
        }
      )
    );
  };
}
