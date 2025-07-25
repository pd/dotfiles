{
  lib,
  modulesPath,
  ...
}:

{
  imports = [
    "${modulesPath}/virtualisation/lxc-container.nix"
    ./orbstack.nix

    ../../modules/core
    ../../users/pd
  ];

  nixpkgs.hostPlatform.system = "x86_64-linux";
  system.stateVersion = "25.05";
  home-manager.users.pd.home.stateVersion = "25.05";

  # Let orbstack own network config
  networking.hostName = "orb";
  lan.enable = false;
  wg.enable = false;

  # move off default port
  services.prometheus.exporters.node = {
    port = 19100;
    listenAddress = lib.mkForce "0.0.0.0";
  };

  users.users.pd = {
    uid = 501;
    extraGroups = [
      "wheel"
      "orbstack"
    ];

    # simulate isNormalUser, but with an arbitrary UID
    isNormalUser = lib.mkForce false;
    isSystemUser = true;
    group = "users";
    createHome = true;
    home = "/home/pd";
    homeMode = "700";
    useDefaultShell = true;
  };

  security.sudo.wheelNeedsPassword = false;

  # This being `true` leads to a few nasty bugs, change at your own risk!
  users.mutableUsers = false;

  time.timeZone = "America/Chicago";

  networking = {
    dhcpcd.enable = false;
    useDHCP = false;
    useHostResolvConf = false;
  };

  systemd.network = {
    enable = true;
    networks."50-eth0" = {
      matchConfig.Name = "eth0";
      networkConfig = {
        DHCP = "ipv4";
        IPv6AcceptRA = true;
      };
      linkConfig.RequiredForOnline = "routable";
    };
  };

  # Extra certificates from OrbStack.
  security.pki.certificates = [
    ''
            -----BEGIN CERTIFICATE-----
      MIICDDCCAbKgAwIBAgIQf+fxBdf00ma1UxF2NjzEgzAKBggqhkjOPQQDAjBmMR0w
      GwYDVQQKExRPcmJTdGFjayBEZXZlbG9wbWVudDEeMBwGA1UECwwVQ29udGFpbmVy
      cyAmIFNlcnZpY2VzMSUwIwYDVQQDExxPcmJTdGFjayBEZXZlbG9wbWVudCBSb290
      IENBMB4XDTI0MDMwODE1MDIzMFoXDTM0MDMwODE1MDIzMFowZjEdMBsGA1UEChMU
      T3JiU3RhY2sgRGV2ZWxvcG1lbnQxHjAcBgNVBAsMFUNvbnRhaW5lcnMgJiBTZXJ2
      aWNlczElMCMGA1UEAxMcT3JiU3RhY2sgRGV2ZWxvcG1lbnQgUm9vdCBDQTBZMBMG
      ByqGSM49AgEGCCqGSM49AwEHA0IABEpbBgLULaGuaAT1bxCMjKoCXFqaozIRRcYr
      Y/uXf5GegTqSSKlPKmfQX5qOG75e84AJ3MKeUn9UOFX/lTRPlTqjQjBAMA4GA1Ud
      DwEB/wQEAwIBBjAPBgNVHRMBAf8EBTADAQH/MB0GA1UdDgQWBBQ6mxIGAl/vWEso
      IL2gEZXZ4WX87DAKBggqhkjOPQQDAgNIADBFAiEA2g6NaDXyEe2JTXYObDUoGg4a
      E+cqjB5fG0aNcYl/fQ4CID1E8JETcVYBlUyUzOQV/bocrkUhG7jBCTcePyqqOlOl
      -----END CERTIFICATE-----

    ''
  ];
}
