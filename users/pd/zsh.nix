{ pkgs, ... }:
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
      syntaxHighlighting.enable = true;

      envExtra = ''
        if [[ -n "$INSIDE_EMACS" ]]; then
          export EDITOR='emacsclient --alternate-editor="" --reuse-frame';
        else
          export EDITOR=vim
        fi

        export PSQLRC="$HOME/.config/pg/psqlrc"
      '';

      shellAliases = {
        em = "emacsclient --alternate-editor='' --no-wait --reuse-frame";
        g = "git";
        ll = "ls -l";
        ls = "ls -Fh";
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
