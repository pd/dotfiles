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
    if pkgs.stdenv.hostPlatform.isLinux then
      "${config.home.homeDirectory}/.config/sops/age/keys.txt"
    else
      "${config.home.homeDirectory}/Library/Application Support/sops/age/keys.txt";

  news = {
    display = "silent";
    entries = lib.mkForce [ ];
  };

  home.username = "pd";
  home.homeDirectory = if pkgs.stdenv.hostPlatform.isDarwin then "/Users/pd" else "/home/pd";
  home.packages =
    with pkgs;
    [
      age
      age-plugin-yubikey
      btop
      cfssl
      file
      graphviz
      htop
      ipcalc
      pwgen
      ssh-to-age
      unzip
      watchexec
    ]
    ++ (with unstable; [
      just
      mise
      nil
      nixfmt
      postgresql
      sops
    ]);

  nix.gc = {
    automatic = true;
    options = "--delete-older-than 15d";
  };
}
