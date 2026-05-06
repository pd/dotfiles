{ lib, pkgs, ... }:
let
  ruby' = pkgs.unstable.ruby.withPackages (
    gems: with gems; [
      pry
      activesupport
    ]
  );
in
{
  home.packages = with pkgs.unstable; [
    delve
    dive
    duckdb
    go
    (lib.hiPrio gopls) # win over gotools `modernize`
    gotools
    go-jsonnet
    lldb
    nil
    (lib.hiPrio ruby') # win over gotools `bundle`
    rustup
    supabase-cli
    uv
  ];
}
