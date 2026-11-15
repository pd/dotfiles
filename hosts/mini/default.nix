{ pd, ... }: {
  system.stateVersion = 7;
  nixpkgs.hostPlatform.system = "aarch64-darwin";

  imports = [
    # ./system.nix
    # ./network.nix
    # ./homebrew.nix
  ];

  networking.hostName = "mini";
  system.primaryUser = "pd";
  users.users.pd = {
    home = /Users/pd;
    openssh.authorizedKeys.keys = pd.keys.workstations.ssh;
  };

  homebrew = {
    enable = true;
    global.autoUpdate = false;
    caskArgs.appdir = "~/Applications";

    casks = [
      "firefox"
      "ghostty"
    ];
  };

  nix.linux-builder = {
    enable = true;
    ephemeral = true;
    maxJobs = 8;
    config.virtualisation = {
      cores = 8;
      darwin-builder = {
        memorySize = 8 * 1024;
        diskSize = 50 * 1024;
      };
    };
    config.users.users.builder.openssh.authorizedKeys.keys = pd.keys.pi.root.ssh;
  };
}
