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
    ./system.nix
  ];

  networking.hostName = "span";

  work.enable = true;
  services.nix-daemon.enable = true;
  security.pam.enableSudoTouchIdAuth = true;

  environment = {
    systemPackages = with pkgs; [
      opentofu
      pandoc
      unstable.postgresql
      wireguard-go
      wireguard-tools

      (texliveBasic.withPackages (
        ps: with ps; [
          bookmark
          etoolbox
          fancyhdr
          float
          hyperref
          lastpage
          listings
          sectsty
          titlesec
          tocloft
          unicode-math
          xcolor
          xetex
        ]
      ))
    ];

    variables = {
      EDITOR = "nvim";
    };
  };

  # half of this should already be installed via brew, but just
  # slamming more shit in here until no more tofu i guess
  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-cjk-sans
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
