{ lib, pd }:
with lib;
let
  inherit (pd.net) lan;

  pad =
    id:
    let
      s = toString id;
    in
    if (mod (stringLength s) 2) == 1 then "0${s}" else s;

  mkv6 =
    host:
    if host.duid != null then
      {
        inherit (host) duid;
        hostid = pad host.id;
      }
    else
      { };

  mkHost =
    name: host:
    {
      inherit name;
      ip = host.lan.ipv4;
      mac = map toUpper host.macs;
    }
    // (mkv6 host);

  hosts = mapAttrsToList mkHost (removeAttrs lan.hosts [ "wrt" ]);
in
{
  dnsmasq = [
    {
      interface = [ "lan" ];
      localservice = true;
      rebind_protection = false;
      server = [
        lan.ipv4.pi
        lan.ipv4.htpc
      ];

      leasefile = "/tmp/dhcp.leases";
      resolvfile = "/tmp/resolv.conf.d/resolv.conf.auto";
    }
  ];

  dhcp.wan = {
    interface = "wan";
    ignore = true;
  };

  dhcp.lan = {
    interface = "lan";
    leasetime = "12h";

    # v4
    dhcpv4 = "server";
    start = 612; # .2.100 .. .2.249
    limit = 150;
    dhcp_option = [
      "6,${lan.ipv4.pi},${lan.ipv4.htpc}"
      "119,home"
    ];

    # v6
    dhcpv6 = "server";
    ra = "server";
    ra_default = true;
    ra_flags = [
      "managed-config"
      "other-config"
    ];
    ra_useleasetime = true;
    dns = [
      lan.ipv6.pi
      lan.ipv6.htpc
    ];
  };

  odhcpd.odhcpd = {
    maindhcp = false;
    leasefile = "/tmp/hosts/odhcpd";
    leasetrigger = "/usr/sbin/odchpd-update";
    loglevel = 4;
  };

  host = hosts;
}
