{
  dmerge,
  net,
  uci,
  ...
}:
let
  # TODO not sure where to encode this
  pi6 = "2600:1700:3040:1e2f::13";
in
{
  rule = dmerge.append [
    {
      name = "wg6";
      target = "ACCEPT";
      src = "wan";
      proto = "udp";
      family = "ipv6";
      dest_ip = [ pi6 ];
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
