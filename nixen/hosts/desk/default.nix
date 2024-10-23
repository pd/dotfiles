{
  lib,
  pkgs,
  config,
  dotfiles,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/base.nix
    ../../modules/wg/client.nix
  ];

  system.stateVersion = "24.05";

  sops.secrets.wifi = {
    mode = "0440";
    owner = "root";
    group = "wheel";
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  time.timeZone = "America/Chicago";

  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  networking = {
    hostName = "desk";

    nameservers = [
      # htpc.lan.ip
      "1.1.1.1"
      "2606:4700:4700::1111"
      "8.8.8.8"
    ];
    search = [ "home" ];

    # prefer networkmanager because at&t router won't let me
    # opt out of their bogus DNS responses
    wireless.enable = false;
    networkmanager = {
      enable = true;
      ensureProfiles.environmentFiles = [
        config.sops.secrets.wifi.path
      ];
      ensureProfiles.profiles = {
        wifi = {
          connection = {
            id = "$ssid";
            autoconnect = "yes";
            permissions = "";
            type = "wifi";
            interface-name = "wlp6s0";
          };
          ipv4 = {
            method = "auto";
            ignore-auto-dns = true;
          };
          ipv6 = {
            method = "auto";
            ignore-auto-dns = true;
          };
          wifi = {
            mode = "infrastructure";
            ssid = "bazqux";
          };
          wifi-security = {
            auth-alg = "open";
            key-mgmt = "wpa-psk";
            psk = "$psk";
          };
        };
      };
    };
  };

  # caps -> ctrl/esc
  # TODO probably simpler: https://wiki.nixos.org/wiki/Keyd
  services.interception-tools = {
    enable = true;
    plugins = [ pkgs.interception-tools-plugins.caps2esc ];
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
    cfssl
    interception-tools
    screen
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

    programs.home-manager.enable = true;
    home.packages = [ pkgs.home-manager ];

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
        append = "commit --amend -C HEAD";
        b = "branch";
        bv = "branch -v";
        co = "checkout";
        ci = "commit";
        cp = "cherry-pick";
        d = "diff";
        ds = "diff --staged";
        l = "log";
        m = "merge";
        pp = "pull --prune";
        rb = "rebase";
        rup = "remote update --prune";
        sh = "show";
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

      matchBlocks."pi" = {
        hostname = "192.168.1.13";
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
      config = {
        modifier = "Mod4";
        terminal = "kitty";
      };
    };

  };
}
