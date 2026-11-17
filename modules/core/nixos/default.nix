{
  config,
  pd,
  pkgs,
  ...
}:
{
  imports = [
    ../.
    ./agenix.nix
    ../../network
    ../../monitoring
  ];

  networking.firewall.enable = true;

  environment.enableAllTerminfo = true;

  documentation.man.cache.enable = config.docs.enable;

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
