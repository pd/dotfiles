{ config, pkgs, ... }:
let
  ruby' = pkgs.ruby.withPackages (gems: with gems; [ pry ]);
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
    zig
    zig-shell-completions
    zls
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
