{
  lib,
  net,
  uci,
  ...
}:
uci.mkRouter "rpt" ./secrets.yaml {
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

    wireless = with uci.wifi; {
      wifi-device.radio0 = device "platform/soc@0/c000000.wifi" // bands."2g" // off;
      wifi-device.radio1 = device "platform/soc@0/b00a040.wifi1" // bands."5g";

      wifi-iface."radio0_sta" = sta "radio0" // wds;
      wifi-iface."radio0_ap" = ap "radio0";

      wifi-iface."radio1_sta" = sta "radio1" // wds;
      wifi-iface."radio1_ap" = ap "radio1";
    };
  };
}
