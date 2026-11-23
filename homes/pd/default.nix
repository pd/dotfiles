{
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./emacs.nix
    ./files.nix
    ./firefox.nix
    ./git.nix
    ./nvim.nix
    ./ops.nix
    ./prog.nix
    ./ssh.nix
    ./zsh.nix
  ];

  programs.home-manager.enable = true;
  programs.man.generateCaches = pkgs.stdenv.hostPlatform.isLinux;

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
      yubikey-manager
    ]
    ++ (with unstable; [
      bitwarden-cli
      just
      mise
      nil
      nixfmt
      postgresql
      usage
    ]);

  nix.gc = {
    automatic = true;
    options = "--delete-older-than 15d";
  };
}
