{
  dmerge,
  pd,
  ...
}:
let
  forwardRule =
    name:
    {
      ip,
      port,
      family ? "ipv6",
      proto ? [
        "tcp"
        "udp"
      ],
    }@_:
    {
      inherit name proto family;
      target = "ACCEPT";
      src = "wan";
      dest = "*";
      dest_ip = [ ip ];
      dest_port = port;
    };

  dnat =
    name:
    {
      ip,
      port,
      proto ? [
        "tcp"
        "udp"
      ],
    }@_:
    {
      inherit name proto;
      src = "wan";
      dest = "lan";
      target = "DNAT";
      src_dport = port;
      dest_ip = ip;
      dest_port = port;
    };
in
{
  rule = dmerge.append [
    (forwardRule "wg6" {
      proto = "udp";
      ip = pd.net.hosts.pi.pub.ipv6;
      port = 51930;
    })
  ];

  redirect = [
    (dnat "www" {
      ip = pd.net.lan.ipv4.htpc;
      port = 443;
    })

    (dnat "rtorrent" {
      ip = pd.net.lan.ipv4.nas;
      port = 50000;
    })

    (dnat "wg" {
      proto = [ "udp" ];
      ip = pd.net.lan.ipv4.pi;
      port = 51930;
    })
  ];
}
