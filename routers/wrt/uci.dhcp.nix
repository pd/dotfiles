{ lib, ... }:
with lib;
let
  # TODO why isn't this available to the fn ^^
  net = import ../../modules/net.nix { inherit lib; };

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

  hosts = mapAttrsToList mkHost (removeAttrs net.lan.hosts [ "wrt" ]);
in
{
  dnsmasq = [
    {
      interface = [ "lan" ];

      server = [ "/*.home/${net.lan.ipv4.pi}" ];
      rebind_domain = [ "home" ];
      domain = "home";
      local = "/home/";

      authoritative = true;
      domainneeded = true;
      expandhosts = true;
      localservice = true;
      rebind_protection = true;
      readethers = true;

      cachesize = 1000;
      ednspacket_max = 1232;

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
      "6,${net.lan.ipv4.pi},${net.lan.ipv4.htpc}"
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
    dns = [
      net.lan.ipv6.pi
      net.lan.ipv6.htpc
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
