{ pkgs, ... }:
let
  paths = map (script: pkgs.writeShellScriptBin script (builtins.readFile ./${script}.sh)) [
    "media-prune"
    "media-sort"
    "retag"
    "lstags"
  ];
in
pkgs.buildEnv {
  name = "mediaman";
  inherit paths;
}
