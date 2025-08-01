{
  host,
  path,

  dewclaw,
  dmerge,
  lib,
  pkgs,
}:
let
  deploy = pkgs.callPackage dewclaw {
    inherit pkgs;
    configuration = {
      openwrt.${host} = import path {
        inherit (lib) pd;
        inherit
          dmerge
          lib
          pkgs
          ;
      };
    };
  };

  # Extract the path to the config file so we can install
  # a copy of it for manual inspection.
  config = pkgs.runCommand "extract-config" { } ''
    config="$(cat ${deploy}/bin/deploy-${host} | grep 'cp.*no-preserve' | awk '{print $3}')"
    install -Dm600 $config $out/${host}.uci
  '';
in
pkgs.buildEnv {
  name = host;
  paths = [
    deploy
    config
  ];
}
