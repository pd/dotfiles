{
  config,
  inputs,
  net,
  pkgs,
  ...
}:
{
  system.stateVersion = 5;

  imports = [
    ../../modules/core/nix.nix
    ../../modules/core/packages.nix
    ../../modules/core/shell.nix

    ../../users/pd
    "${inputs.private}/work"
  ];

  work.enable = true;

  environment.variables = {
    EDITOR = "vim";
  };

  nix = {
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];

      trusted-users = [ "@admin" ];
    };

    gc = {
      automatic = true;
    };
  };

  services.nix-daemon.enable = true;

  security.pam.enableSudoTouchIdAuth = true;

  networking.hostName = "span";

  system = {
    activationScripts.postUserActivation.text = ''
      # activateSettings -u will reload the settings from the database and
      # apply them to the current session, so we do not need to logout and
      # login again to make the changes take effect.
      /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
    '';

    defaults = {
      NSGlobalDomain."com.apple.swipescrolldirection" = false;
      dock = {
        autohide = true;
        orientation = "bottom";
        show-recents = false;
      };
    };

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };

  environment.systemPackages = with pkgs; [
    wireguard-go
    wireguard-tools
  ];

  # Mostly use homebrew for now, to approximate my current system
  homebrew = {
    enable = true;

    global.autoUpdate = false;

    caskArgs = {
      appdir = "~/Applications";
    };

    casks = [
      "alfred"
      "bitwarden"
      "dash"
      "discord"
      "firefox"
      "iterm2"
      "orbstack"
      "neo4j"
      "notion"
      "signal"
      "slack"
      "spectacle"
      "visual-studio-code"
      "vlc"
      "wireshark"
      "zoom"
    ];

    brews = [ "restish" ];
  };

  # half of this should already be installed via brew, but just
  # slamming more shit in here until no more tofu i guess
  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    fira-code
    fira-code-symbols
    liberation_ttf
    nerdfonts
    proggyfonts
  ];

  users.users.pd = {
    home = /Users/pd;
  };

  home-manager.users.pd = {
    home.stateVersion = "24.05";
    home.packages = [ pkgs.unstable.emacs30 ];

    # Keep orbstack in the ssh config
    programs.ssh.includes = [ "~/.orbstack/ssh/config" ];

    sops.defaultSopsFile = ./secrets.yaml;
    sops.age.keyFile = "/Users/pd/Library/Application Support/sops/age/keys.txt";
    sops.secrets.wireguard-private-key = { };
    sops.secrets.wireguard-preshared-key = { };
  };

  networking.wg-quick.interfaces.wg0 =
    let
      secrets = config.home-manager.users.pd.sops.secrets;
    in
    {
      privateKeyFile = secrets.wireguard-private-key.path;
      address = [ "${net.hosts.span.wg.ip}/32" ];
      dns = [ "${net.hosts.pi.wg.ip},wg,home" ];
      postDown = "networksetup -setdnsservers Wi-Fi Empty";

      peers = [
        {
          endpoint = "wg.krh.me:51820";
          publicKey = net.hosts.donix.wg.publicKey;
          presharedKeyFile = secrets.wireguard-preshared-key.path;
          allowedIPs = [ "10.100.100.0/24" ];
          persistentKeepalive = 25;
        }
      ];
    };
}
