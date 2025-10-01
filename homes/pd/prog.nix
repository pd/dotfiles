{ pkgs, ... }:
let
  ruby' = pkgs.unstable.ruby.withPackages (
    gems: with gems; [
      pry
      activesupport
    ]
  );

  # use zvm because nixpkgs often has zig+zls diverge, and then most
  # of zls is hosed. but stop telling me there's an upgrade available
  zvm = pkgs.symlinkJoin {
    name = "zvm";
    paths = [ pkgs.unstable.zvm ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/zvm --set ZVM_SET_CU off
    '';
  };
in
{
  home.packages = with pkgs.unstable; [
    delve
    dive
    go
    gopls
    gotools
    go-jsonnet
    lldb
    nil
    (hiPrio ruby') # win over gotools `bundle`
    supabase-cli
    uv

    zvm
  ];
}
