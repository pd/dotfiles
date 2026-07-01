{ inputs, pkgs, ... }:
{
  imports = [
    ../pd
    ./work.nix
    "${inputs.private}/pd@armspan"
  ];

  home.stateVersion = "25.05";

  programs.ssh.includes = [ "~/.orbstack/ssh/config" ];

  home.packages = [
    pkgs.nixos-rebuild-ng
  ];
}
