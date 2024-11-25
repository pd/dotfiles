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

    ./homebrew.nix
  ];

  work.enable = true;

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
      dock = {
        autohide = true;
        orientation = "bottom";
        mru-spaces = false;
        show-recents = false;
      };

      loginwindow = {
        autoLoginUser = "pd";
        GuestEnabled = false;
      };

      menuExtraClock = {
        Show24Hour = true;
        ShowDate = 1;
        ShowDayOfWeek = true;
        ShowSeconds = false;
      };

      NSGlobalDomain = {
        AppleInterfaceStyle = "Dark";
        AppleInterfaceStyleSwitchesAutomatically = false;

        # stop pretending to be a phone
        "com.apple.swipescrolldirection" = false;

        # no beep when changing volume
        "com.apple.sound.beep.feedback" = 0;

        # 24h time
        AppleICUForce24HourTime = true;
      };

      trackpad = {
        TrackpadThreeFingerTapGesture = 0;
      };
    };

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };

    # shush
    startup.chime = false;
  };

  environment = {
    systemPackages = with pkgs; [
      unstable.postgresql
      wireguard-go
      wireguard-tools
    ];

    variables = {
      EDITOR = "nvim";
    };
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
    home.packages = [
      pkgs.kyverno-chainsaw
    ];

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
      address = [
        "${net.wg.ipv4.span}/32"
        "${net.wg.ipv6.span}/128"
      ];
      dns = [ "${net.wg.ipv4.pi},${net.wg.ipv6.pi},wg,home" ];
      postDown = "networksetup -setdnsservers Wi-Fi Empty";

      peers = [
        {
          endpoint = "wg.krh.me:51930";
          publicKey = net.wg.pks.pi;
          presharedKeyFile = secrets.wireguard-preshared-key.path;
          allowedIPs = [
            net.lan.cidr
            net.lan.cidr6
            net.wg.cidr
            net.wg.cidr6
          ];
          persistentKeepalive = 25;
        }
      ];
    };
}
