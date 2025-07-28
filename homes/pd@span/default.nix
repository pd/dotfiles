{ inputs, ... }:
{
  imports = [
    ../pd
    "${inputs.private}/work"
  ];

  home.stateVersion = "25.05";
  home.homeDirectory = "/Users/pd";

  work.enable = true;

  programs.ssh.includes = [ "~/.orbstack/ssh/config" ];
}
