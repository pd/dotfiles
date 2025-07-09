{
  dmerge,
  net,
  uci,
  ...
}:
{
  rule = dmerge.append [
    {
      name = "wg6";
      target = "ACCEPT";
      src = "wan";
      dest = "*";
      proto = "udp";
      family = "ipv6";
      dest_ip = [ net.hosts.pi.pub.ipv6 ];
      dest_port = 51930;
    }
  ];

  redirect = [
    (uci.dnat "rtorrent" {
      ip = net.lan.ipv4.nas;
      port = 50000;
    })

    (uci.dnat "wg" {
      proto = [ "udp" ];
      ip = net.lan.ipv4.pi;
      port = 51930;
    })
  ];
}
