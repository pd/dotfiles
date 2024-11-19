{ pkgs, ... }:
let
  ruby' = pkgs.ruby.withPackages (gems: with gems; [ pry ]);
in
{
  home-manager.users.pd = {
    home.packages = with pkgs.unstable; [
      go
      gopls
      gotools
      go-jsonnet
      nil
      (hiPrio ruby') # win over gotools `bundle`
      zig
      zig-shell-completions
      zls
    ];
  };
}
