{ uci, ... }:
{
  globals = [ { ula_prefix = "fded:1::/48"; } ];

  interface.loopback = {
    device = "lo";
    ipaddr = "127.0.0.1";
    netmask = "255.0.0.0";
    proto = "static";
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
  };

  interface.lan = {
    device = "br-lan";
    proto = "static";
    ipaddr = "192.168.1.1";
    netmask = "255.255.252.0";
    ip6assign = 64;
    dns = [
      "fded:1::13"
      "192.168.1.13"
      "fded:1::12"
      "192.168.1.12"
    ];
    dns_search = [ "home" ];
  };

  device = [
    {
      name = "eth1";
      macaddr = "94:83:c4:a3:31:20";
    }
  ] ++ (uci.bridgeLan 5 "94:83:c4:a3:31:22");
}
