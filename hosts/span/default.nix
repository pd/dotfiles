{
  inputs,
  pkgs,
  ...
}:
{
  system.stateVersion = 5;
  nixpkgs.hostPlatform.system = "x86_64-darwin";

  imports = [
    ../../modules/core/nix.nix
    ../../modules/core/packages.nix
    ../../modules/core/shell.nix

    ../../users/pd
    "${inputs.private}/work"

    ./system.nix
    ./network.nix
    ./homebrew.nix
  ];

  networking.hostName = "span";

  work.enable = true;
  security.pam.services.sudo_local.touchIdAuth = true;

  environment = {
    systemPackages = with pkgs; [
      opentofu
      pandoc
      unstable.postgresql
      wireguard-go
      wireguard-tools

      (texliveBasic.withPackages (
        ps: with ps; [
          bookmark
          etoolbox
          fancyhdr
          float
          hyperref
          lastpage
          listings
          sectsty
          titlesec
          tocloft
          unicode-math
          xcolor
          xetex
        ]
      ))
    ];

    variables = {
      EDITOR = "nvim";
    };
  };

  # half of this should already be installed via brew, but just
  # slamming more shit in here until no more tofu i guess
  fonts.packages =
    with pkgs;
    [
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-emoji
      fira-code
      fira-code-symbols
      liberation_ttf
      proggyfonts
    ]
    ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);

  users.users.pd = {
    home = /Users/pd;
  };

  home-manager.users.pd = {
    home.stateVersion = "24.05";
    home.packages = [
      pkgs.kyverno-chainsaw
    ];

    programs.ssh.includes = [ "~/.orbstack/ssh/config" ];

    sops.defaultSopsFile = ./secrets.yaml;
    sops.age.keyFile = "/Users/pd/Library/Application Support/sops/age/keys.txt";
    sops.secrets.wireguard-private-key = { };
    sops.secrets.wireguard-preshared-key = { };
  };
}
