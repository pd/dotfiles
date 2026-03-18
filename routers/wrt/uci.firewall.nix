{
  dmerge,
  pd,
  ...
}:
let
  forwardRule =
    name:
    {
      ip ? null,
      port,
      family ? "ipv6",
      proto ? [
        "tcp"
        "udp"
      ],
    }:
    {
      inherit name proto family;
      target = "ACCEPT";
      src = "wan";
      dest = "*";
      dest_port = port;
    }
    // (if ip != null then { dest_ip = [ ip ]; } else { });

  dnat =
    name:
    {
      ip,
      port,
      proto ? [
        "tcp"
        "udp"
      ],
    }:
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
      port = 51930;
    })
  ];

  redirect = [
    (dnat "www" {
      proto = [ "tcp" ];
      ip = pd.net.lan.ipv4.htpc;
      port = 443;
    })

    (dnat "qbittorrent" {
      ip = pd.net.lan.ipv4.htpc;
      port = 51000;
    })

    (dnat "wg" {
      proto = [ "udp" ];
      ip = pd.net.lan.ipv4.pi;
      port = 51930;
    })
  ];
}
