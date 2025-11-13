{ inputs, ... }:
{
  imports = [
    ../pd
    "${inputs.private}/pd@armspan"
  ];

  home.stateVersion = "25.05";
  home.homeDirectory = "/Users/pd";

  programs.ssh.includes = [ "~/.orbstack/ssh/config" ];
}
