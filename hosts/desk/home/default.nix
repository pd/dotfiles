{ pkgs, ... }:
{
  imports = [
    ../../../users/pd
    ./emacs.nix
    ./wm.nix
  ];

  home-manager.users.pd = {
    home.stateVersion = "24.05";

    programs.home-manager.enable = true;
    programs.firefox.enable = true;

    home.packages =
      with pkgs;
      [
        opentofu
        pavucontrol # audio
        pinta # remedial image editing
        pyrosimple # rtorrent clis
        screen
      ]
      ++ (with unstable; [
        go
        gotools
        signal-desktop
        slack
      ]);
  };

  # Needed for age-plugin-yubikey
  services.pcscd.enable = true;
}
