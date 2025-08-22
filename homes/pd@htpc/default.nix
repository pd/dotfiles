{ pkgs, ... }:
{
  imports = [ ../pd ];

  home.stateVersion = "25.05";
  home.homeDirectory = "/home/pd";
  home.packages = [
    pkgs.pd.xtor
  ];
}
