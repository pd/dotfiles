{
  dmerge,
  lib,

  authorized-keys,
  resolvers,
  ...
}:
with lib;
{
  bridgeLan =
    n: macaddr:
    let
      devs = map (i: {
        inherit macaddr;
        name = "lan${toString i}";
      }) (range 1 n);
    in
    devs
    ++ [
      {
        name = "br-lan";
        type = "bridge";
        ports = catAttrs "name" devs;
      }
    ];

  wifi = rec {
    bands = {
      "2g" = {
        band = "2g";
        channel = 1;
        htmode = "HE40";
      };
      "5g" = {
        band = "5g";
        channel = 36;
        htmode = "HE80";
      };
    };

    device = path: {
      inherit path;
      type = "mac80211";
      country = "US";
      cell_density = "1";
    };

    iface = device: {
      inherit device;
      network = "lan";
      ssid = lib.pd.net.wifi.ssid;
      encryption = "psk2";
      key._secret = "wifi_password";
    };

    sta = device: (iface device) // { mode = "sta"; };
    ap =
      device:
      (iface device)
      // {
        mode = "ap";
        bss_transition = true; # 802.11v
        ieee80211r = true;
        mobility_domain = "dead";
      };

    off = {
      disabled = true;
    };

    wds = {
      wds = true;
    };
  };

  mkRouter =
    hostname: secrets: custom:
    dmerge.merge {
      deploy = {
        host = "${hostname}.home";
        sshConfig.Port = 1222;
      };

      sopsSecrets = secrets;
      users.root.hashedPassword = "$6$VaxrusIClFD3RwYc$GP9rU3UrVrn5Qz1PrtN716jWEAeYte1Lj6eq.NcY1iupk0f35P8MeiRhe7L0EkVrxNC0OT2Uah1VzJBwdJJav1";
      etc."dropbear/authorized_keys".text = concatStringsSep "\n" authorized-keys;

      uci.settings = {
        dropbear.dropbear = [
          {
            Interface = "lan";
            Port = 1222;
            PasswordAuth = "off";
          }

          {
            Interface = "lan6";
            Port = 1222;
            PasswordAuth = "off";
          }
        ];

        system = {
          system = [
            {
              inherit hostname;
              zonename = "America/Chicago";
            }
          ];

          timeserver.ntp = {
            enabled = true;
            enable_server = false;
            server = [
              "0.openwrt.pool.ntp.org"
              "1.openwrt.pool.ntp.org"
              "2.openwrt.pool.ntp.org"
              "3.openwrt.pool.ntp.org"
            ];
          };
        };

        network = {
          interface.loopback = {
            device = "lo";
            proto = "static";
            ipaddr = "127.0.0.1";
            netmask = "255.0.0.0";
          };

          interface.lan = {
            device = "br-lan";
            proto = "static";
            netmask = "255.255.252.0";
            dns = resolvers;
            dns_search = [ "home" ];
          };
        };

        firewall = {
          defaults = [
            {
              input = "REJECT";
              output = "ACCEPT";
              forward = "REJECT";
              synflood_protect = true;
            }
          ];

          zone = [
            {
              name = "lan";
              input = "ACCEPT";
              output = "ACCEPT";
              forward = "ACCEPT";
              network = [ "lan" ];
              log = false;
            }

            {
              name = "wan";
              input = "REJECT";
              output = "ACCEPT";
              forward = "REJECT";
              network = [
                "wan"
                "wan6"
              ];
              masq = true;
              mtu_fix = true;
              log = false;
            }
          ];

          forwarding.lan_wan = {
            src = "lan";
            dest = "wan";
          };

          rule = import ./default-firewall-rules.nix;
        };

        prometheus-node-exporter-lua = {
          prometheus-node-exporter-lua.main = {
            listen_interface = "lan";
            listen_ipv6 = true;
            listen_port = 9100;
          };
        };
      };

      # I don't want dewclaw managing packages at all.
      # TODO: This is not a particularly public interface, so is very
      # likely to break at some point.
      deploySteps.packages = {
        copy = lib.mkForce "";
        apply = lib.mkForce "";
      };

    } custom;
}
