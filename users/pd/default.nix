{ pkgs, ... }:
{
  imports = [
    ./git.nix
    ./zsh.nix
  ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };

  home-manager.users.pd = {
    programs.home-manager.enable = true;

    home.packages = with pkgs; [
      age
      age-plugin-yubikey
      cfssl
      just
      nil
      nixfmt-rfc-style
      sops
      watchexec
    ];
  };
}
