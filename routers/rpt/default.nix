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
        ignore = true;
        interface = "lan";
      };

      dhcp.lan6 = {
        ignore = true;
        interface = "lan6";
      };
    };

    network = {
      device = lib.uci.bridgeLan 2 (lib.head hosts.rpt.macs);
      interface.lan = {
        device = "br-lan";
        proto = "dhcp";
      };

      interface.lan6 = {
        device = "br-lan";
        proto = "dhcpv6";
        reqaddress = "try";
        reqprefix = "auto";
        norelease = true;
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
