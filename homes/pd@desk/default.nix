{ pkgs, ... }:
{
  imports = [
    ../pd
    ./emacs.nix
    ./wm.nix
  ];

  home.stateVersion = "25.05";
  home.homeDirectory = "/home/pd";

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

  # connect to orbstack nixos vm by jumping through span
  programs.ssh.matchBlocks.orb = {
    proxyJump = "span";
    hostname = "localhost";
    port = 32222;
  };
}
