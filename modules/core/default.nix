# Things I'm guaranteed to want on every system.

{
  config,
  pd,
  pkgs,
  ...
}:
{
  imports = [
    ../network
    ./monitoring.nix
    ./nix.nix
    ./packages.nix
    ./shell.nix
  ];

  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  sops.defaultSopsFile = ../../hosts/${config.networking.hostName}/secrets.yaml;

  networking.firewall.enable = true;

  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "en_US.UTF-8/UTF-8" ];
  };

  services.openssh = {
    enable = true;
    ports = [ 1222 ];
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      KbdInteractiveAuthentication = false;
    };
  };

  security.sudo.wheelNeedsPassword = false;
  users = {
    mutableUsers = false;
    users.pd = {
      isNormalUser = true;
      extraGroups = [
        "keys"
        "networkmanager"
        "wheel"
      ];
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = pd.keys.workstations.ssh;
    };
  };
}
