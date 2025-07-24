{
  host,
  path,
  authorized-keys,

  dewclaw,
  dmerge,
  lib ? pkgs.lib,
  net,
  pkgs,
}:
let
  uci = import ./uci.nix {
    inherit
      authorized-keys
      dmerge
      lib
      net
      ;
  };

  deploy = pkgs.callPackage dewclaw {
    inherit pkgs;
    configuration = {
      openwrt.${host} = import path {
        inherit
          dmerge
          lib
          net
          pkgs
          uci
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
