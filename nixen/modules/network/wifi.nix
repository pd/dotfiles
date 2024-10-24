{ lib, config, ... }:
{
  options = {
    wifi.enable = lib.mkEnableOption "Use wifi";
    wifi.interface = lib.mkOption {
      type = lib.types.str;
    };
  };

  config = lib.mkIf config.wifi.enable {
    sops.secrets.wifi = {
      mode = "0440";
      owner = "root";
      group = "wheel";
    };

    networking = {
      nameservers = [
        "192.168.1.13" # pi
        "1.1.1.1"
        "2606:4700:4700::1111"
        "8.8.8.8"
      ];

      search = [ "home" ];
      wireless.enable = false;
      networkmanager = {
        enable = true;

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
              interface-name = config.wifi.interface;
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
    };
  };
}
