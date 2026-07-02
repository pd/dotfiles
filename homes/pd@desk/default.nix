{
  config,
  inputs,
  pkgs,
  ...
}:
{
  imports = [
    ../pd
    "${inputs.private}/pd@desk"
    ./wm.nix
    ./music.nix
  ];

  home.stateVersion = "25.05";

  programs.firefox = {
    enable = true;
    configPath = "${config.xdg.configHome}/mozilla/firefox";
  };

  home.packages =
    with pkgs;
    [
      discord # god help me
      pavucontrol # audio
      pinta # remedial image editing
      screen
      zeal
      zoom-us

      # depends on EOL electron, and i don't particularly need it
      # https://github.com/bitwarden/clients/pull/20448
      # https://github.com/nixos/nixpkgs/issues/526914
      # bitwarden-desktop
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
