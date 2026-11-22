{ pd, ... }: {
  system.stateVersion = 7;
  nixpkgs.hostPlatform.system = "aarch64-darwin";

  networking.hostName = "mini";
  system.primaryUser = "pd";
  users.users.pd = {
    home = /Users/pd;
    openssh.authorizedKeys.keys = pd.keys.workstations.ssh;
  };

  networking.homeULA = {
    enable = true;
    interface = "en0";
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

  services.prometheus.exporters.node = {
    enable = true;
    # https://github.com/prometheus/node_exporter/issues/2906
    # https://github.com/prometheus/node_exporter/pull/3767
    disabledCollectors = [ "thermal" ];
  };

  power.sleep.computer = "never";
  power.restartAfterPowerFailure = true;
}
