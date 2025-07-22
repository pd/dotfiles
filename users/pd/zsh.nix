{ pkgs, ... }:
let
  em = pkgs.writeShellScriptBin "em" (
    if pkgs.hostPlatform.isLinux then
      ''
        emacsclient --alternate-editor="" --no-wait --reuse-frame "$@"
      ''
    else
      # Inexplicably, `--reuse-frame` does the opposite on MacOS (as of Emacs 30).
      # It's not just me, apparently:
      # https://emacs.stackexchange.com/questions/79292/why-is-emacsclient-not-reusing-the-existing-frame
      ''
        if emacsclient -n -e "(if (> (length (frame-list)) 1) 't)" | grep t >/dev/null 2>&1; then
          emacsclient --alternate-editor="" --no-wait "$@"
        else
          emacsclient --alternate-editor="" --create-frame --no-wait "$@"
        fi
      ''
  );
in
{
  # If using my user's zshrc, no need for compinit to run from
  # the system zshrc.
  programs.zsh = {
    enableGlobalCompInit = false;
    enableBashCompletion = false;

    # to include global startup in zprof:
    # shellInit = "zmodload zsh/zprof";
  };

  home-manager.users.pd =
    { config, ... }:
    {
      home.packages = [ em ];

      programs.autojump = {
        enable = true;
        enableZshIntegration = true;
      };

      programs.direnv = {
        enable = true;
        enableZshIntegration = true;
        nix-direnv.enable = true;
      };

      programs.fzf = {
        enable = true;
        enableZshIntegration = true;
      };

      programs.zsh = {
        enable = true;
        # zprof.enable = true;

        autocd = true;
        enableVteIntegration = true;

        history = {
          save = 50000;
          size = 10000;
          path = "${config.xdg.cacheHome}/zsh/history";

          expireDuplicatesFirst = true;
          ignoreDups = true;
          ignoreAllDups = true;
          ignoreSpace = true;
          saveNoDups = true;
          share = true;
        };

        envExtra = ''
          if [[ -n "$INSIDE_EMACS" ]]; then
            export EDITOR="emacsclient"
          else
            export EDITOR=nvim
          fi

          export PSQLRC="$HOME/.config/pg/psqlrc"
          export KUBECTL_EXTERNAL_DIFF='dyff between --omit-header --set-exit-code'

          export LESS="--quit-if-one-screen --RAW-CONTROL-CHARS"

          # not everything is nix'd yet
          export PATH="$HOME/bin:$HOME/go/bin:$PATH"
        '';

        initContent = ''
          set -o emacs

          # man 1 zshoptions
          setopt cdablevars
          setopt glob_star_short
          setopt hist_reduce_blanks
          setopt inc_append_history
          unsetopt nomatch

          [[ -f "${config.xdg.configHome}/zsh/p10k.zsh" ]] && source "${config.xdg.configHome}/zsh/p10k.zsh"
        '';

        shellAliases = {
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
            file = "powerlevel10k.zsh-theme";
            src = "${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k";
          }
          {
            name = "zhooks";
            file = "zhooks.plugin.zsh";
            src = pkgs.fetchFromGitHub {
              owner = "agkozak";
              repo = "zhooks";
              rev = "e6616b4a2786b45a56a2f591b79439836e678d22";
              sha256 = "zahXMPeJ8kb/UZd85RBcMbomB7HjfEKzQKjF2NnumhQ=";
            };
          }
        ];
      };
    };
}
