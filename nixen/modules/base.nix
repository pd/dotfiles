# Things I'm guaranteed to want on every system.

{ pkgs, ... }: let
  keys = import ./keys.nix;
in {
  sops.age.sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.trusted-users = ["root" "@wheel"];
  security.sudo.wheelNeedsPassword = false;

  networking.firewall.enable = true;

  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "en_US.UTF-8/UTF-8" ];
  };

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    curl
    dig
    fd
    git
    htop
    jq
    procps
    ripgrep
    tree
    vim
  ];

  programs.zsh.enable = true;

  services.openssh = {
    enable = true;
    ports = [ 1222 ];
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      KbdInteractiveAuthentication = false;
    };
  };

  users = {
    mutableUsers = false;
    users.pd = {
      isNormalUser = true;
      extraGroups = ["networkmanager" "wheel" "keys"];
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = keys.desk.ssh ++ keys.span.ssh;
    };
  };

}
