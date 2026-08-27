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

  programs.ghostty = {
    enable = true;
    package = null; # via homebrew instead
    settings = {
      background = "#101010";
      unfocused-split-fill = "#5a5a5a";
      clipboard-paste-protection = false;

      keybind = [
        "global:cmd+opt+enter=new_window"
      ];
    };
  };
}
