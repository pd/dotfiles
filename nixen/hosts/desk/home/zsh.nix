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

    # https://github.com/direnv/direnv/wiki/Sops/bdc3484a0603120cdbec7cdc0d4daf218f2c4ca0
    xdg.configFile."direnv/direnvrc" = {
      enable = true;
      text = ''
        use_sops() {
          local path="''${1:-$PWD/.envrc.sops.yaml}"
          eval "$(sops -d --output-type dotenv "$path" | direnv dotenv bash /dev/stdin)"
          watch_file "$path"
        }
      '';
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

      sessionVariables = {
        EDITOR = "emacsclient --alternate-editor='' --reuse-frame";
      };

      shellAliases = {
        g = "git";
        z = "j";
        em = "emacsclient --alternate-editor='' --no-wait --reuse-frame";
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
