{ pkgs, pkgs-unstable, ... }:
{
  home-manager.users.pd = {
    programs.autojump = {
      enable = true;
      enableZshIntegration = true;
    };

    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
    };

    programs.fzf = {
      enable = true;
      enableZshIntegration = true;
    };

    programs.zsh = {
      enable = true;

      enableVteIntegration = true;
      autocd = true;

      envExtra = ''
        # Ensure tramp can parse the prompt
        [[ $TERM = "dumb" ]] && unsetopt zle && PS1='$ '
      '';

      initExtra = ''
        source ${pkgs-unstable.emacsPackages.vterm}/share/emacs/site-lisp/elpa/*/etc/emacs-vterm-zsh.sh
      '';

      shellAliases = {
        g = "git";
        z = "j";
      };

      plugins = [
        {
          name = "up";
          src = pkgs.fetchFromGitHub {
            owner = "cjayross";
            repo = "up";
            rev = "b8f11253753be845aec450c3ac68f317a0c0ec2a";
            sha256 = "McOiMwLclsZ11PzyvNeMnSK7oiuticpTPKRKb3v8At8=";
          };
        }
      ];
    };
  };
}
