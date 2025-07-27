{ pkgs, ... }:
let
  prune = pkgs.writeShellScriptBin "media-prune" (builtins.readFile ./media-prune.sh);
  sort = pkgs.writeShellScriptBin "media-sort" (builtins.readFile ./media-sort.sh);
in
pkgs.buildEnv {
  name = "mediaman";
  paths = [
    prune
    sort
  ];
}
