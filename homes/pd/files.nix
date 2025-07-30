{ pkgs, ... }:
let
  binFile = name: builtins.readFile ../../bin/${name};
  mkScript = name: pkgs.writeShellScriptBin name (binFile name);

  scripts = map mkScript [
    "bcopsup"
    "ctx"
    "ff-container"
    "git-show-file-at-rev"
    "git-take-file-from-rev"
    "utc"
  ];

  etc = {
    "direnv/direnvrc" = ./etc/direnvrc;
    "irb/irbrc" = ./etc/irbrc;
    "mise/config.toml" = ./etc/mise.toml;
    "mise/go" = ./etc/mise-go-packages;
    "pry/pryrc" = ./etc/pryrc;
    "pg/psqlrc" = ./etc/psqlrc;
    "sqlite3/sqliterc" = ./etc/sqliterc;
  };
in
{
  home.packages = scripts;
  xdg.configFile = builtins.mapAttrs (dest: source: { inherit source; }) etc;
}
