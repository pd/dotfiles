{ pkgs, ... }:
{
  imports = [ ../pd ];

  home.stateVersion = "26.05";
  home.packages = [
    pkgs.pd.xtor
  ];
}
