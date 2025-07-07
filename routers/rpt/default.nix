{
  lib,
  net,
  uci,
  ...
}:
uci.mkRouter "rpt" [ ] {
  deploy.host = net.lan.ipv4.rpt; # TODO why does ipv6 (thus by hostname) fail

  uci.retain = [
    "luci"
    "rpcd"
    "ttyd"
    "ubootenv"
    "uhttpd"
  ];

  uci.settings = {
    dhcp = {
      dnsmasq = [
        {
          interface = [ "lan" ];
          localservice = true;
          rebind_protection = false;
          server = [
            net.lan.ipv4.pi
            net.lan.ipv4.htpc
          ];
        }
      ];

      dhcp.lan = {
        interface = "lan";
        ignore = true;
      };
    };

    network = {
      device = uci.bridgeLan 2 (lib.head net.hosts.rpt.macs);
      interface.lan = {
        ipaddr = net.lan.ipv4.rpt;
        gateway = net.lan.ipv4.wrt;
        ip6addr = [ "${net.lan.ipv6.rpt}/64" ];
        ip6gw = [ net.lan.ipv6.wrt ];
      };
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
