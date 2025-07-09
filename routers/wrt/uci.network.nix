{ net, uci, ... }:
{
  globals = [ { ula_prefix = "fded:40::/48"; } ];

  device = [
    {
      name = "eth1";
      macaddr = "94:83:c4:a3:31:20";
    }
  ] ++ (uci.bridgeLan 5 "94:83:c4:a3:31:22");

  interface.lan = {
    ipaddr = net.lan.ipv4.wrt;
    ip6addr = "${net.lan.ipv6.wrt}/64";
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
