{ inputs, pkgs, pkgs-unstable, ... }:
{
  system.stateVersion = 5;

  imports = [
    "${inputs.private}/work"
    ../../users/pd
  ];

  work.enable = true;

  environment.variables = {
    EDITOR = "vim";
  };

  nix = {
    package = pkgs.nix;

    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];

      trusted-users = [ "@admin" ];
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
      "iterm2"
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

    brews = [ "restish" ];
  };

  services.emacs = {
    enable = true;
    package = pkgs-unstable.emacs30;
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

  users.users.pd = {
    home = /Users/pd;
  };

  home-manager.users.pd = {
    home.stateVersion = "24.05";
    home.packages = [ pkgs-unstable.emacs30 ];
  };
}
