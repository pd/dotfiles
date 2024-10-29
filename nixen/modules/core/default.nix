# Things I'm guaranteed to want on every system.

{ pkgs, config, ... }:
let
  keys = import ../keys.nix;
in
{
  imports = [
    ../network
  ];

  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  sops.defaultSopsFile = ../../hosts/${config.networking.hostName}/secrets.yaml;

  nix = {
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];

      trusted-users = [
        "root"
        "@wheel"
      ];
    };

    gc = {
      automatic = true;
      persistent = true;
      dates = "weekly";
      randomizedDelaySec = "30min";
      options = "--delete-older-than 30d";
    };
  };
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
    tcpdump
    tree
    vim
  ];

  programs.zsh = {
    enable = true;
    interactiveShellInit = ''
      autoload -U select-word-style
      select-word-style bash
    '';
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

  users = {
    mutableUsers = false;
    users.pd = {
      isNormalUser = true;
      extraGroups = [
        "networkmanager"
        "wheel"
        "keys"
      ];
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = keys.desk.ssh ++ keys.span.ssh;
    };
  };
}
