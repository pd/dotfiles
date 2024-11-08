{ pkgs, ... }:
{
  home-manager.users.pd =
    { config, ... }:
    {
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

      home.packages = [ pkgs.zsh-powerlevel10k ];

      programs.zsh = {
        enable = true;

        autocd = true;
        enableVteIntegration = true;

        history = {
          save = 50000;
          size = 10000;
          path = "$HOME/.history.zsh";

          expireDuplicatesFirst = true;
          ignoreDups = true;
          ignoreAllDups = true;
          ignoreSpace = true;
          share = true;
        };

        envExtra = ''
          if [[ -n "$INSIDE_EMACS" ]]; then
            export EDITOR='emacsclient --alternate-editor="" --reuse-frame';
          else
            export EDITOR=nvim
          fi

          export PSQLRC="$HOME/.config/pg/psqlrc"
          export KUBECTL_EXTERNAL_DIFF='dyff between --omit-header --set-exit-code'

          # not everything is nixied up yet
          export PATH="$HOME/bin:$HOME/go/bin:$PATH"
        '';

        initExtra =
          let
            xdg = config.xdg;
          in
          ''
            # man 1 zshoptions
            setopt cdablevars
            setopt glob_star_short
            setopt hist_reduce_blanks
            setopt hist_save_no_dups
            setopt inc_append_history
            unsetopt nomatch

            if which mise &>/dev/null; then
              source <(mise activate zsh)
            fi

            [[ -f "${xdg.configHome}/zsh/p10k.zsh" ]] && source "${xdg.configHome}/zsh/p10k.zsh"
          '';

        shellAliases = {
          em = "emacsclient --alternate-editor='' --no-wait --reuse-frame";
          g = "git";
          k = "kubectl";
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
          {
            name = "powerlevel10k";
            src = "${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k";
            file = "powerlevel10k.zsh-theme";
          }
        ];
      };
    };
}
