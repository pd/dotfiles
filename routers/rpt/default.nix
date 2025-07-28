{
  lib,
  pd,
  ...
}:
let
  inherit (pd.net) lan hosts;
in
lib.uci.mkRouter "rpt" ./secrets.yaml {
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
            lan.ipv6.pi
            lan.ipv4.pi
            lan.ipv6.htpc
            lan.ipv4.htpc
          ];
        }
      ];

      dhcp.lan = {
        interface = "lan";
        ignore = true;
        dhcpv6 = "relay";
        ra = "relay";
      };
    };

    network = {
      device = lib.uci.bridgeLan 2 (lib.head hosts.rpt.macs);
      interface.lan = {
        ipaddr = [ "${lan.ipv4.rpt}/22" ];
        gateway = lan.ipv4.wrt;
        ip6addr = [ "${lan.ipv6.rpt}/64" ];
        ip6gw = lan.ipv6.wrt;
      };
    };

    wireless = with lib.uci.wifi; {
      wifi-device.radio0 = device "platform/soc@0/c000000.wifi" // bands."2g" // off;
      wifi-device.radio1 = device "platform/soc@0/b00a040.wifi1" // bands."5g";

      wifi-iface."radio0_sta" = sta "radio0" // wds;
      wifi-iface."radio0_ap" = ap "radio0";

      wifi-iface."radio1_sta" = sta "radio1" // wds;
      wifi-iface."radio1_ap" = ap "radio1";
    };
  };
}
