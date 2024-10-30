{
  config,
  pkgs,
  pkgs-unstable,
  ...
}:
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
      pkgs-unstable.go
      pkgs-unstable.gotools
      just
      pyrosimple
      screen
      sops
    ];
  };
}
