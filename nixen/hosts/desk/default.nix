{
  lib,
  pkgs,
  config,
  dotfiles,
  ...
} : {
  imports = [
    ./hardware-configuration.nix
    ../../modules/base.nix
    ../../modules/wg/client.nix
  ];

  system.stateVersion = "24.05";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = ["ntfs"];

  networking.hostName = "desk";
  networking.networkmanager.enable = true;

  time.timeZone = "America/Chicago";

  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  services.interception-tools = {
    enable = true;
    plugins = [pkgs.interception-tools-plugins.caps2esc];
    udevmonConfig = ''
      - JOB: "${pkgs.interception-tools}/bin/intercept -g $DEVNODE | ${pkgs.interception-tools-plugins.caps2esc}/bin/caps2esc -m 1 | ${pkgs.interception-tools}/bin/uinput -d $DEVNODE"
        DEVICE:
          EVENTS:
            EV_KEY: [KEY_CAPSLOCK, KEY_ESC]
    '';
  };

  # TODO: can i turn this back on?
  # things want a password to log in after screenlock etc
  users.mutableUsers = lib.mkForce true;

  environment.systemPackages = with pkgs; [
    age
    interception-tools
    sops

    # TODO figurin out wm
    kitty
    mako
    wl-clipboard
  ];

  security.polkit.enable = true;
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd sway";
        user = config.users.users.pd.name;
      };

      initial_session = {
        command = "sway";
        user = config.users.users.pd.name;
      };
    };
  };

  home-manager.users.pd = {
    home.stateVersion = "24.05";

    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
    };

    programs.emacs = {
      enable = true;
      package = pkgs.emacs30-pgtk;
    };

    services.emacs = {
      enable = true;
      defaultEditor = true;
    };

    home.file.".emacs.d" = {
      source = "${dotfiles}/emacs.d";
      recursive = true;
    };

    programs.firefox.enable = true;

    programs.fzf = {
      enable = true;
      enableZshIntegration = true;
    };

    programs.git = {
      enable = true;
      userName = "Kyle Hargraves";
      userEmail = "pd@krh.me";

      aliases = {
        b = "branch";
        co = "checkout";
        ci = "commit";
        cp = "cherry-pick";
        d  = "diff";
        rb = "rebase";
        st = "status";
      };

      ignores = [
        ".DS_Store"
        "*.swp"
        ".#*"
        "#*#"
        "*~"
      ];

      extraConfig = {
        diff = {
          algorithm = "patience";
        };

        init = {
          defaultBranch = "main";
        };

        push = {
          default = "upstream";
        };

        status = {
          relativePaths = false;
        };

        url."git@github.com" = {
          insteadOf = "https://github.com";
        };
      };
    };

    programs.go.enable = true;

    programs.ssh = {
      enable = true;
      matchBlocks."donix" = {
        hostname = "donix.krh.me";
        port = 1222;
      };
    };

    programs.zsh = {
      enable = true;
      autocd = true;

      # Ensure tramp can parse the prompt
      envExtra = ''
        [[ $TERM = "dumb" ]] && unsetopt zle && PS1='$ '
      '';

      shellAliases = {
        g = "git";
      };
    };

    wayland.windowManager.sway = {
      enable = true;
      config = rec {
        modifier = "Mod4";
        terminal = "kitty";
      };
    };

  };
}
