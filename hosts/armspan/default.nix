{
  pkgs,
  ...
}:
{
  system.stateVersion = 6;
  nixpkgs.hostPlatform.system = "aarch64-darwin";

  imports = [
    ./system.nix
    ./network.nix
    ./homebrew.nix
  ];

  networking.hostName = "armspan";
  users.users.pd.home = /Users/pd;

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

}
