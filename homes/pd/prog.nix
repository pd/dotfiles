{ config, pkgs, ... }:
let
  ruby' = pkgs.ruby.withPackages (
    gems: with gems; [
      pry
      activesupport
    ]
  );
in
{
  home.packages = with pkgs.unstable; [
    dive
    go
    gopls
    gotools
    go-jsonnet
    nil
    (hiPrio ruby') # win over gotools `bundle`
    supabase-cli
    uv

    # use zvm because nixpkgs often has zig+zls diverge,
    # and then most of zls is hosed
    zvm
  ];

  programs.jujutsu =
    let
      git = config.programs.git;
    in
    {
      enable = true;
      package = pkgs.unstable.jujutsu;
      settings = {
        user.name = git.userName;
        user.email = git.userEmail;

        signing = {
          behavior = "own";
          backend = "ssh";
          key = git.signing.key;
        };
      };
    };
}
