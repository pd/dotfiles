{ pkgs, pkgs-unstable, ... }:
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
      age-plugin-yubikey
      cfssl
      pkgs-unstable.go
      pkgs-unstable.gotools
      just
      opentofu
      pyrosimple
      screen
      pkgs-unstable.signal-desktop
      pkgs-unstable.slack
      sops
      watchexec
    ];
  };

  # Needed for age-plugin-yubikey
  services.pcscd.enable = true;
}
