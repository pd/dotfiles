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
    # let me use nixos-rebuild from darwin, keeping the unprefixed
    # name so i don't have to adjust scripts everywhere
    (pkgs.symlinkJoin {
      name = "nixos-rebuild-ng-aliased";
      paths = [ pkgs.nixos-rebuild-ng ];
      postBuild = "ln -s $out/bin/nixos-rebuild-ng $out/bin/nixos-rebuild";
    })
  ];
}
