{ pd, uci }:
let
  inherit (pd.net) lan hosts;
in
{
  globals = [ { ula_prefix = lan.cidr6; } ];

  device = [
    {
      name = "eth1";
      macaddr = hosts.wrt.macs.wan;
    }
  ]
  ++ (uci.bridgeLan 5 hosts.wrt.macs.lan);

  interface.lan = {
    ipaddr = lan.ipv4.wrt;
    ip6addr = "${lan.ipv6.wrt}/64";
    ip6assign = 64;
    ip6class = [
      "local"
      "wan6"
    ];
  };

  interface.wan = {
    device = "eth1";
    proto = "dhcp";
    peerdns = false;
  };

  interface.wan6 = {
    device = "eth1";
    proto = "dhcpv6";
    reqaddress = "try";
    reqprefix = "auto";
    peerdns = false;
    delegate = true;
  };
}
