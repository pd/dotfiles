{ pkgs, ... }:
{
  imports = [ ../pd ];

  home.stateVersion = "25.05";
  home.packages = [
    pkgs.pd.xtor
  ];
}
