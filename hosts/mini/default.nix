{
  system.stateVersion = 7;
  nixpkgs.hostPlatform.system = "aarch64-darwin";

  imports = [
    # ./system.nix
    # ./network.nix
    # ./homebrew.nix
  ];

  networking.hostName = "mini";
  system.primaryUser = "pd";
  users.users.pd.home = /Users/pd;

  homebrew = {
    enable = true;
    global.autoUpdate = false;
    caskArgs.appdir = "~/Applications";

    casks = [
      "firefox"
      "ghostty"
    ];
  };
}
