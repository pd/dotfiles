{ pkgs, ... }:
let
  binFile = name: builtins.readFile ../../bin/${name};
  mkScript = name: pkgs.writeShellScriptBin name (binFile name);

  scripts = map mkScript [
    "ctx"
    "ff-container"
    "git-show-file-at-rev"
    "git-take-file-from-rev"
    "utc"
  ];
in
{
  home-manager.users.pd.home.packages = scripts;
}
