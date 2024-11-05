{ pkgs, ... }:
{
  system.stateVersion = 5;

  # lol?
  environment.systemPackages = with pkgs; [
    cfssl
    curl
    dig
    dyff
    fd
    git
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

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToControl = true;
  };

  system.defaults.NSGlobalDomain."com.apple.swipescrolldirection" = false;

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
  };
}
