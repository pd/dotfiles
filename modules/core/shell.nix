{ pkgs, ... }:
let
  vtermZsh = pkgs.writeTextFile {
    name = "vterm.plugin.zsh";
    text = builtins.readFile ./vterm.zsh;
  };
in
{
  # Ensure the default shell experience isn't too miserable,
  # especially given emacs+tramp+ssh.
  #
  # Most shell config really lives in home-manager, which is
  # only on my actual workstations.
  programs.zsh = {
    enable = true;

    interactiveShellInit = ''
      autoload -U select-word-style
      select-word-style bash

      setopt autocd
      unsetopt nomatch

      source "${vtermZsh}"
    '';
  };
}
