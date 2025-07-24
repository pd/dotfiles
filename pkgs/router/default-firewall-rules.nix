# https://git.openwrt.org/?p=project/firewall4.git;a=blob;f=root/etc/config/firewall;h=d78a00c28988909971898fdbd275aff4ae94b788;hb=HEAD
[
  {
    name = "Allow-DHCP-Renew";
    target = "ACCEPT";
    src = "wan";
    proto = "udp";
    dest_port = 68;
    family = "ipv4";
  }

  {
    name = "Allow-Ping";
    target = "ACCEPT";
    src = "wan";
    proto = "icmp";
    icmp_type = "echo-request";
    family = "ipv4";
  }

  {
    name = "Allow-IGMP";
    target = "ACCEPT";
    src = "wan";
    proto = "igmp";
    family = "ipv4";
  }

  {
    name = "Allow-DHCPv6";
    target = "ACCEPT";
    src = "wan";
    proto = "udp";
    dest_port = 546;
    family = "ipv6";
  }

  {
    name = "Allow-MLD";
    target = "ACCEPT";
    src = "wan";
    proto = "icmp";
    src_ip = "fe80::/10";
    icmp_type = [
      "130/0"
      "131/0"
      "132/0"
      "143/0"
    ];
    family = "ipv6";
  }

  {
    name = "Allow-ICMPv6-Input";
    target = "ACCEPT";
    src = "wan";
    proto = "icmp";
    limit = "1000/sec";
    family = "ipv6";
    icmp_type = [
      "echo-request"
      "echo-reply"
      "destination-unreachable"
      "packet-too-big"
      "time-exceeded"
      "bad-header"
      "unknown-header-type"
      "router-solicitation"
      "neighbour-solicitation"
      "router-advertisement"
      "neighbour-advertisement"
    ];
  }

  {
    name = "Allow-ICMPv6-Forward";
    target = "ACCEPT";
    src = "wan";
    dest = "*";
    proto = "icmp";
    limit = "1000/sec";
    family = "ipv6";
    icmp_type = [
      "echo-request"
      "echo-reply"
      "destination-unreachable"
      "packet-too-big"
      "time-exceeded"
      "bad-header"
      "unknown-header-type"
    ];
  }

  {
    name = "Allow-IPSec-ESP";
    target = "ACCEPT";
    src = "wan";
    dest = "lan";
    proto = "esp";
  }

  {
    name = "Allow-ISAKMP";
    target = "ACCEPT";
    src = "wan";
    dest = "lan";
    dest_port = 500;
    proto = "udp";
  }
]
