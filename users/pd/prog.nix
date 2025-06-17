{ pkgs, ... }:
let
  ruby' = pkgs.ruby.withPackages (gems: with gems; [ pry ]);
in
{
  home-manager.users.pd = {
    home.packages = with pkgs.unstable; [
      dive
      go
      gopls
      gotools
      go-jsonnet
      nil
      (hiPrio ruby') # win over gotools `bundle`
      uv
      zig
      zig-shell-completions
      zls
    ];
  };
}
