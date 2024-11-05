{ pkgs, ... }:
{
  system.stateVersion = 5;

  # lol?
  environment.systemPackages = with pkgs; [
    curl
    dig
    fd
    git
    htop
    jq
    iterm2
    procps
    ripgrep
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
}
