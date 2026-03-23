{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  inherit (pkgs.stdenv) isLinux;

  hostsNixCache = config.networking.hostName == "htpc";
  substituters = [
    "https://nix-community.cachix.org"
  ]
  ++ (if hostsNixCache then [ ] else [ "http://nix.home" ]);

  trusted-public-keys = [
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ]
  ++ (if hostsNixCache then [ ] else [ "nix.home-1:lDXeyLWCMghXN6ATUouJK1tcAQ0sV8D3yyay98cHhHc=" ]);
in
{
  nix = {
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];

      inherit substituters trusted-public-keys;

      trusted-users = if isLinux then [ "@wheel" ] else [ "@admin" ];
    };

    optimise = mkMerge [
      { automatic = true; }
      (optionalAttrs isLinux { dates = [ "05:25" ]; })
      (optionalAttrs (!isLinux) {
        interval = {
          Hour = 5;
          Minute = 25;
        };
      })
    ];

    gc = mkMerge [
      {
        automatic = true;
        options = "--delete-older-than 30d";
      }

      (optionalAttrs isLinux {
        persistent = true;
        dates = "weekly";
        randomizedDelaySec = "30min";
      })

      (optionalAttrs (!isLinux) {
        interval = {
          Weekday = 0; # Sunday
          Hour = 6;
          Minute = 30;
        };
      })
    ];
  };
}
