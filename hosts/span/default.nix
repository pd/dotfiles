{ inputs, pkgs, ... }:
{
  system.stateVersion = 5;

  imports = [
    "${inputs.private}/work"
  ];

  work.enable = true;

  # lol?
  environment.systemPackages = with pkgs; [
    cfssl
    curl
    dig
    dyff
    fd
    git
    google-cloud-sdk
    htop
    kfilt
    kubectl
    kubernetes-helm
    kustomize
    jq
    just
    iterm2
    mise
    procps
    ripgrep
    stern
    tree
    vim
  ];

  nix = {
    package = pkgs.nix;

    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];

      trusted-users = ["@admin"];
    };

    gc = {
      automatic = true;
    };
  };

  nixpkgs.config.allowUnfree = true;
  services.nix-daemon.enable = true;

  security.pam.enableSudoTouchIdAuth = true;

  system = {
    defaults = {
      NSGlobalDomain."com.apple.swipescrolldirection" = false;
      dock = {
        autohide = true;
        orientation = "bottom";
        show-recents = false;
      };
    };

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };


  # Mostly use homebrew for now, to approximate my current system
  homebrew = {
    enable = true;

    global.autoUpdate = false;

    caskArgs = {
      appdir = "~/Applications";
    };

    casks = [
      "alfred"
      "bitwarden"
      "dash"
      "discord"
      "firefox"
      "orbstack"
      "neo4j"
      "notion"
      "signal"
      "slack"
      "spectacle"
      "visual-studio-code"
      "vlc"
      "wireshark"
      "zoom"
    ];

    brews = [
      "restish"
    ];
  };

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

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };

  users.users.pd = {
    home = /Users/pd;
  };

  home-manager.users.pd = {
    home.stateVersion = "24.05";

    programs.home-manager.enable = true;

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
