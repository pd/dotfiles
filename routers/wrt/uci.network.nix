{ lib, pd, ... }:
{
  globals = [ { ula_prefix = pd.net.lan.cidr6; } ];

  device = [
    {
      name = "eth1";
      macaddr = "94:83:c4:a3:31:20";
    }
  ] ++ (lib.uci.bridgeLan 5 "94:83:c4:a3:31:22");

  interface.lan = {
    ipaddr = pd.net.lan.ipv4.wrt;
    netmask = "255.255.252.0";
    ip6addr = "${pd.net.lan.ipv6.wrt}/64";
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
