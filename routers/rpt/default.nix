{
  lib,
  uci,
  ...
}:
{
  deploy.host = "192.168.1.2"; # TODO why does ssh over ipv6 fail
  deploy.sshConfig = {
    Port = 1222;
  };

  packages = [ ];
  users.root.hashedPassword = "$6$VaxrusIClFD3RwYc$GP9rU3UrVrn5Qz1PrtN716jWEAeYte1Lj6eq.NcY1iupk0f35P8MeiRhe7L0EkVrxNC0OT2Uah1VzJBwdJJav1";
  etc."dropbear/authorized_keys".text = uci.authorized-keys;

  uci.retain = [
    "firewall"
    "luci"
    "rpcd"
    "ttyd"
    "ubootenv"
    "uhttpd"
  ];

  uci.sopsSecrets = ./secrets.yaml;
  uci.settings = {
    dropbear.dropbear = [
      {
        Interface = "lan";
        Port = 1222;
        PasswordAuth = "off";
      }
    ];

    system = {
      system = [
        {
          hostname = "rpt";
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

    dhcp = {
      dnsmasq = [
        {
          localservice = true;
        }
      ];

      dhcp.lan = {
        interface = "lan";
        ignore = true;
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
        ipaddr = "192.168.1.2";
        netmask = "255.255.252.0";
        ip6addr = [ "fded:1::2" ];
        ip6gw = [ "fded:1::1" ];
        dns = [
          "192.168.1.13"
          "192.168.1.12"
        ];
        dns_search = "home";
      };

      device =
        let
          lan-devices = map (n: {
            name = "lan${toString n}";
            macaddr = "94:83:c4:a4:aa:d2";
          }) (lib.range 1 2);
        in
        [
          {
            name = "br-lan";
            type = "bridge";
            ports = map (d: d.name) lan-devices;
          }
        ]
        ++ lan-devices;
    };

    wireless =
      let
        device = {
          type = "mac80211";
          channel = "auto";
          htmode = "HE40";
          cell_density = false;
        };

        iface = {
          network = "lan";
          ssid = "bazqux";
          encryption = "psk2";
          key._secret = "wifi_password";
        };

        ap = iface // {
          mode = "ap";
          ieee80211r = true;
          mobility_domain = "dead";
          ft_over_ds = false;
          ft_psk_generate_local = true;
        };

        sta = iface // {
          mode = "sta";
          wds = true;
        };
      in
      {
        wifi-device.radio0 = device // {
          path = "platform/soc@0/c000000.wifi";
          band = "2g";
        };

        wifi-device.radio1 = device // {
          path = "platform/soc@0/soc@0:wifi1@c000000";
          band = "5g";
        };

        wifi-iface."radio0_sta" = sta // {
          device = "radio0";
          disabled = true;
        };

        wifi-iface."radio0_ap" = ap // {
          device = "radio0";
        };

        wifi-iface."radio1_sta" = sta // {
          device = "radio1";
        };

        wifi-iface."radio1_ap" = ap // {
          device = "radio1";
          disabled = true;
        };
      };
  };
}
