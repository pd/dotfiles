{ lib, config, ... }:
with lib;
let
  cfg = config.lan;
in
{
  options = {
    lan.enable = mkEnableOption "inside the house";

    lan.gateway = mkOption {
      type = types.str;
      default = "192.168.1.254";
    };

    lan.ipv4 = mkOption {
      type = types.str;
    };

    lan.nameservers = mkOption {
      type = types.listOf types.str;
      default = [
        "192.168.1.13" # pi
        "2606:4700:4700::1111"
        "1.1.1.1"
        "8.8.8.8"
      ];
    };

    lan.wired.interface = mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    lan.wifi.interface = mkOption {
      type = types.nullOr types.str;
      default = null;
    };
  };

  config = mkMerge [
    {
      networking.search = [ "home" ];
      networking.nameservers = cfg.nameservers;
    }

    (mkIf (cfg.enable && cfg.wired.interface != null) {
      networking.defaultGateway.interface = cfg.wired.interface;
      networking.defaultGateway.address = cfg.gateway;
      networking.interfaces."${cfg.wired.interface}".ipv4.addresses = [
        {
          address = cfg.ipv4;
          prefixLength = 24;
        }
      ];
    })

    (mkIf (cfg.enable && cfg.wifi.interface != null) {
      sops.secrets.wifi = {
        mode = "0440";
        owner = "root";
        group = "wheel";
      };

      networking.wireless.enable = false;

      networking.networkmanager = {
        enable = true;

        # I don't (currently) run nixos on anything with a battery,
        # this just hurts wifi performance for no benefit.
        wifi.powersave = false;

        ensureProfiles.environmentFiles = [
          config.sops.secrets.wifi.path
        ];

        ensureProfiles.profiles = {
          wifi = {
            connection = {
              id = "$ssid";
              autoconnect = "yes";
              permissions = "";
              type = "wifi";
              interface-name = cfg.wifi.interface;
            };
            ipv4 = {
              method = "auto";
              ignore-auto-dns = true;
            };
            ipv6 = {
              method = "auto";
              ignore-auto-dns = true;
            };
            wifi = {
              mode = "infrastructure";
              ssid = "$ssid";
            };
            wifi-security = {
              auth-alg = "open";
              key-mgmt = "wpa-psk";
              psk = "$psk";
            };
          };
        };
      };
    })
  ];
}
