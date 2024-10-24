{
  lib,
  pkgs,
  config,
  ...
}:
let
  net = import ../../modules/net.nix;
in
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/base.nix
    ../../modules/network
    ./wm.nix
  ];

  system.stateVersion = "24.05";

  networking.hostName = "desk";
  lan.wifi.interface = "wlp6s0";

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

  # TODO: can i turn this back on?
  # things want a password to log in after screenlock etc
  users.mutableUsers = lib.mkForce true;

  environment.systemPackages = with pkgs; [
    age
    cfssl
    interception-tools
    screen
    sops
  ];

  # TOD: do i still need this
  security.polkit.enable = true;

  home-manager.users.pd = {config, pkgs, ...}: {
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
      startWithUserSession = true;
    };

    home.file = let
      homeDir = "/home/pd";
      dot = f: { source = config.lib.file.mkOutOfStoreSymlink "${homeDir}/dotfiles/${f}"; };
    in
    {
      ".emacs.d" = dot "emacs.d";
    };

    programs.home-manager.enable = true;
    home.packages = [
      pkgs.home-manager
      pkgs.emacsPackages.vterm
    ];

    programs.firefox.enable = true;

    programs.fzf = {
      enable = true;
      enableZshIntegration = true;
    };

    programs.autojump = {
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

      matchBlocks = {
        donix = {
          hostname = "donix.krh.me";
          port = 1222;
        };

        span = {
          hostname = "span.home";
          identitiesOnly = true;
          identityFile = "~/.ssh/id_ed25519";
        };

      } // lib.mapAttrs' (name: host: {
        inherit name;
        value = {
          hostname = "${name}.home";
          port = 1222;
          identitiesOnly = true;
          identityFile = "~/.ssh/id_ed25519";
        };
      }) (lib.filterAttrs (name: _: builtins.elem name ["htpc" "pi" "nas"]) net.hosts);
    };

    programs.zsh = {
      enable = true;

      enableVteIntegration = true;
      autocd = true;

      # Ensure tramp can parse the prompt
      envExtra = ''
        [[ $TERM = "dumb" ]] && unsetopt zle && PS1='$ '
        export SOPS_AGE_KEY_FILE="$HOME/.age/key-primary-ed25519.txt"
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
