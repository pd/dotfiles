# Things I'm guaranteed to want on every system.

{ config, pkgs, ... }:
let
  keys = import ../keys.nix;
in
{
  imports = [
    ../network
    ./monitoring.nix
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

  networking.firewall.enable = true;

  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "en_US.UTF-8/UTF-8" ];
  };

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

      if [[ "$TERM" != "dumb" ]]; then
        source ${pkgs.emacsPackages.vterm}/share/emacs/site-lisp/elpa/*/etc/emacs-vterm-zsh.sh
        add-zsh-hook -Uz chpwd() {
          if [[ -n "$SSH_CONNECTION" ]]; then
            vterm_cmd update-default-directory "/ssh:$(hostname):$PWD/"
          else
            vterm_cmd update-default-directory "$PWD/"
          fi
          print -Pn "\e]2;%m:%2~\a"
        }
      fi
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

  security.sudo.wheelNeedsPassword = false;
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
