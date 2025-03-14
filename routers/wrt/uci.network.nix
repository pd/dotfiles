{ net, uci, ... }:
{
  globals = [ { ula_prefix = "fded:1::/48"; } ];

  device = [
    {
      name = "eth1";
      macaddr = "94:83:c4:a3:31:20";
    }
  ] ++ (uci.bridgeLan 5 "94:83:c4:a3:31:22");

  interface.lan = {
    ipaddr = net.lan.ipv4.wrt;
    ip6assign = 64;
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
