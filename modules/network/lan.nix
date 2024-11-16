{
  config,
  lib,
  net,
  ...
}:
with lib;
let
  cfg = config.lan;
in
{
  options = {
    lan.enable = mkEnableOption "inside the house";

    lan.gateway = mkOption {
      type = types.str;
      default = "192.168.1.1";
    };

    lan.ipv4 = mkOption { type = types.str; };

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
    (mkIf (cfg.enable && cfg.wired.interface != null) {
      networking.interfaces."${cfg.wired.interface}" = {
        name = cfg.wired.interface;
        useDHCP = true;
        # TODO wakeOnLan
      };
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

        ensureProfiles.environmentFiles = [ config.sops.secrets.wifi.path ];
        ensureProfiles.profiles.wifi = {
          connection = {
            id = "$ssid";
            autoconnect = "yes";
            permissions = "";
            type = "wifi";
            interface-name = cfg.wifi.interface;
          };
          ipv4 = {
            method = "auto";
          };
          ipv6 = {
            method = "auto";
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
    })
  ];
}
