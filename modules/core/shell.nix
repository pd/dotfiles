{ pkgs, ... }:
{
  # Ensure the default shell experience isn't too miserable,
  # especially over ssh.
  #
  # Most shell config really lives in home-manager, which is
  # only on my actual workstations.
  programs.zsh = {
    enable = true;
    interactiveShellInit = ''
      autoload -U select-word-style
      select-word-style bash

      if [[ "$TERM" != "dumb" ]]; then
        source ${pkgs.emacsPackages.vterm}/share/emacs/site-lisp/elpa/*/etc/emacs-vterm-zsh.sh
        add-zsh-hook -Uz chpwd() {
          if [[ -n "$SSH_CONNECTION" ]]; then
            vterm_cmd update-default-directory "/ssh:$(hostname):$PWD/"
          else
            vterm_cmd update-default-directory "$PWD/"
          fi
          print -Pn "\e]2;%m:%2~\a"
        }
      fi
    '';
  };
}
