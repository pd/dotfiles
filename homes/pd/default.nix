{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./emacs.nix
    ./files.nix
    ./git.nix
    ./nvim.nix
    ./ops.nix
    ./prog.nix
    ./ssh.nix
    ./zsh.nix
  ];

  programs.home-manager.enable = true;

  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile =
    if pkgs.hostPlatform.isLinux then
      "${config.home.homeDirectory}/.config/sops/age/keys.txt"
    else
      "${config.home.homeDirectory}/Library/Application Support/sops/age/keys.txt";

  news = {
    display = "silent";
    entries = lib.mkForce [ ];
  };

  home.username = "pd";
  home.packages =
    with pkgs;
    [
      age
      age-plugin-yubikey
      cfssl
      htop
      ipcalc
      just
      ssh-to-age
      unzip
      watchexec
    ]
    ++ (with unstable; [
      mise
      nil
      nixfmt-rfc-style
      postgresql
      sops
    ]);

  nix.gc = {
    automatic = true;
    options = "--delete-older-than 15d";
  };

  programs.ghostty = {
    enable = true;
    package = if pkgs.hostPlatform.isDarwin then pkgs.unstable.ghostty-bin else pkgs.ghostty;
    enableZshIntegration = true;
    settings = {
      auto-update = "off";
      background = "111111";
      foreground = "fdf4c1";
      split-divider-color = "747372";
      font-family = "FiraCode Nerd Font Mono";
      font-size = 12;
    };
  };
}
