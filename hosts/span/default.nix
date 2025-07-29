{
  pkgs,
  ...
}:
{
  system.stateVersion = 5;
  nixpkgs.hostPlatform.system = "x86_64-darwin";

  imports = [
    ./system.nix
    ./network.nix
    ./homebrew.nix
  ];

  networking.hostName = "span";

  security.pam.services.sudo_local.touchIdAuth = true;

  environment = {
    systemPackages = with pkgs; [
      pandoc
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
}
