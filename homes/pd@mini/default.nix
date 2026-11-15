{ pkgs, ... }:
{
  imports = [
    ../pd
  ];

  xdg.enable = true;

  home.stateVersion = "26.05";
  home.packages = [
    pkgs.nixos-rebuild-ng
  ];
}
