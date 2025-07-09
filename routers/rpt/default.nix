{
  lib,
  net,
  uci,
  ...
}:
uci.mkRouter "rpt" [ ] {
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
        dhcpv6 = "disabled";
        ra = "disabled";
      };
    };

    network = {
      device = uci.bridgeLan 2 (lib.head net.hosts.rpt.macs);
      interface.lan = {
        ipaddr = [ "${net.lan.ipv4.rpt}/22" ];
        gateway = net.lan.ipv4.wrt;
        ip6addr = [ "${net.lan.ipv6.rpt}/64" ];
        ip6gw = net.lan.ipv6.wrt;
      };
    };

    wireless =
      let
        device = {
          type = "mac80211";
        };

        iface = {
          network = "lan";
          ssid = "bazqux";
          encryption = "psk2";
          key._secret = "wifi_password";
        };

        sta = iface // {
          mode = "sta";
          wds = true;
        };

        ap = iface // {
          mode = "ap";
        };
      in
      {
        wifi-device.radio0 = device // {
          path = "platform/soc@0/c000000.wifi";
          band = "2g";
          channel = 1;
          htmode = "HE20";
          disabled = true;
        };

        wifi-device.radio1 = device // {
          path = "platform/soc@0/b00a040.wifi1";
          band = "5g";
          channel = 36;
          htmode = "HE80";
        };

        wifi-iface."radio0_sta" = sta // {
          device = "radio0";
        };

        wifi-iface."radio0_ap" = ap // {
          device = "radio0";
        };

        wifi-iface."radio1_sta" = sta // {
          device = "radio1";
        };

        wifi-iface."radio1_ap" = ap // {
          device = "radio1";
        };
      };
  };
}
