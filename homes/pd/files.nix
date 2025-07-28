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

  etc = {
    "direnv/direnvrc" = ../../users/pd/etc/direnvrc;
    "irb/irbrc" = ../../users/pd/etc/irbrc;
    "mise/config.toml" = ../../users/pd/etc/mise.toml;
    "mise/go" = ../../users/pd/etc/mise-go-packages;
    "pry/pryrc" = ../../users/pd/etc/pryrc;
    "pg/psqlrc" = ../../users/pd/etc/psqlrc;
    "sqlite3/sqliterc" = ../../users/pd/etc/sqliterc;
  };
in
{
  home.packages = scripts;
  xdg.configFile = builtins.mapAttrs (dest: source: { inherit source; }) etc;
}
