{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.lan;
in
{
  options.lan = {
    enable = mkEnableOption "inside the house";

    # TODO rm once everything's cut over
    networkd = mkEnableOption "use systemd-networkd";

    gateway = mkOption {
      type = types.str;
      default = "192.168.40.1";
    };

    ipv4 = mkOption { type = types.str; };

    wired.interface = mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    wifi.interface = mkOption {
      type = types.nullOr types.str;
      default = null;
    };
  };

  config = mkMerge [
    {
      # TODO irrelevant now i think? move to resolvectl somehow i guess
      networking.resolvconf.extraOptions = [ "timeout:1" ];
    }

    (mkIf cfg.networkd {
      networking.useDHCP = false;
      systemd.network.enable = true;
    })

    (mkIf (cfg.enable && cfg.networkd && cfg.wired.interface != null) {
      systemd.network.networks."10-lan" = {
        matchConfig.Name = cfg.wired.interface;
        networkConfig = {
          DHCP = true;
          UseDomains = true;
          IPv6AcceptRA = true;
        };

        dhcpV4Config = {
          RouteMetric = 100;
        };

        ipv6AcceptRAConfig = {
          RouteMetric = 100;
        };
      };
    })

    (mkIf (cfg.enable && !cfg.networkd && cfg.wired.interface != null) {
      networking.interfaces.${cfg.wired.interface} = {
        name = cfg.wired.interface;
        useDHCP = true;
      };
    })

    (mkIf (cfg.enable && cfg.networkd && cfg.wifi.interface != null) {
      sops.secrets.wifi = {
        mode = "0440";
        owner = "root";
        group = "wheel";
      };

      systemd.network.networks."20-wifi" = {
        matchConfig.Name = cfg.wifi.interface;
        networkConfig = {
          DHCP = true;
          IPv6AcceptRA = true;
          UseDomains = true;
        };

        dhcpV4Config = {
          RouteMetric = 300;
        };

        ipv6AcceptRAConfig = {
          RouteMetric = 300;
        };
      };

      networking.networkmanager.enable = false;
      networking.wireless.iwd = {
        enable = true;
        settings = {
          Network.EnableIPv6 = true;
          Settings.AutoConnect = true;
        };
      };
    })

    (mkIf (cfg.enable && !cfg.networkd && cfg.wifi.interface != null) {
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
            autoconnect = "true";
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
