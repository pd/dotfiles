{ pkgs, ... }:
{
  imports = [
    ../../../users/pd
    ./emacs.nix
    ./ssh.nix
    ./wm.nix
  ];

  home-manager.users.pd = {
    home.stateVersion = "24.05";

    programs.home-manager.enable = true;
    programs.firefox.enable = true;

    home.packages = with pkgs; [
      git-trim
      pkgs.unstable.go
      pkgs.unstable.gotools
      opentofu
      pyrosimple
      screen
      pkgs.unstable.signal-desktop
      pkgs.unstable.slack
    ];
  };

  # Needed for age-plugin-yubikey
  services.pcscd.enable = true;
}
