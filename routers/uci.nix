{
  dmerge,
  lib,
  net,
  ...
}:
with lib;
let
  keys = import ../modules/keys.nix;
  authorized-keys = concatStringsSep "\n" (keys.desk.ssh ++ keys.span.ssh);
in
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

  dnat =
    name:
    {
      ip,
      port,
      proto ? [
        "tcp"
        "udp"
      ],
    }@_:
    {
      inherit name proto;
      src = "wan";
      dest = "lan";
      target = "DNAT";
      src_dport = port;
      dest_ip = ip;
      dest_port = port;
    };

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

    off = {
      disabled = true;
    };
    wds = {
      wds = true;
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
      ssid = "bazqux";
      encryption = "psk2";
      key._secret = "wifi_password";
    };

    ap = device: (iface device) // { mode = "ap"; };
    sta = device: (iface device) // { mode = "sta"; };
  };

  mkRouter =
    hostname: packages: custom:
    dmerge.merge {
      inherit packages;

      deploy = {
        host = "${hostname}.home";
        sshConfig.Port = 1222;
      };

      users.root.hashedPassword = "$6$VaxrusIClFD3RwYc$GP9rU3UrVrn5Qz1PrtN716jWEAeYte1Lj6eq.NcY1iupk0f35P8MeiRhe7L0EkVrxNC0OT2Uah1VzJBwdJJav1";
      etc."dropbear/authorized_keys".text = authorized-keys;

      uci.sopsSecrets = ./${hostname}/secrets.yaml;

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
            dns = [
              net.lan.ipv6.pi
              net.lan.ipv4.pi
              net.lan.ipv6.htpc
              net.lan.ipv4.htpc
            ];
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
      };

    } custom;
}
