{ inputs, pkgs, ... }:
{
  imports = [
    ../pd
    "${inputs.private}/pd@desk"
    ./wm.nix
    ./music.nix
  ];

  home.stateVersion = "25.05";
  home.homeDirectory = "/home/pd";

  programs.firefox.enable = true;

  home.packages =
    with pkgs;
    [
      bitwarden-cli
      bitwarden-desktop
      discord # god help me
      pavucontrol # audio
      pinta # remedial image editing
      screen
      zeal
      zoom-us
    ]
    ++ (with unstable; [
      aws-workspaces
      signal-desktop
      slack
    ]);

  # connect to orbstack nixos vm by jumping through span
  programs.ssh.matchBlocks.orb = {
    proxyJump = "span";
    hostname = "localhost";
    port = 32222;
  };

  # desk is only linux box that isn't headless
  programs.nixvim.clipboard.providers.wl-copy.enable = true;
}
