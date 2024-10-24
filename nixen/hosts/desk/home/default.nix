{ config, pkgs, ... }:
{
  imports = [
    ./emacs.nix
    ./git.nix
    ./ssh.nix
    ./wm.nix
    ./zsh.nix
  ];

  home-manager.users.pd = {
    home.stateVersion = "24.05";

    programs.home-manager.enable = true;

    programs.firefox.enable = true;

    home.packages = with pkgs; [
      age
      cfssl
      go
      just
      screen
    ];
  };
}
