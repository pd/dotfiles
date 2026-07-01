{ inputs, pkgs, ... }:
{
  imports = [
    ../pd
    "${inputs.private}/pd@desk"
    ./wm.nix
    ./music.nix
  ];

  home.stateVersion = "25.05";

  programs.firefox.enable = true;

  home.packages =
    with pkgs;
    [
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

  # connect to orbstack nixos vm by jumping through armspan
  programs.ssh.settings.orb = {
    ProxyJump = "armspan";
    HostName = "localhost";
    Port = 32222;
  };

  # desk is only linux box that isn't headless
  programs.nixvim.clipboard.providers.wl-copy.enable = true;
}
