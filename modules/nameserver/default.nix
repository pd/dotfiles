{
  lib,
  pd,
  pkgs,
  ...
}:
let
  dnsInfo = tld: net: name: host: {
    inherit (host.${net}) ipv4;
    name = "${name}.${tld}";
    cnames = map (n: "${n}.${tld}") (host.cnames or [ ]);
    ipv6 = host.${net}.ipv6 or null;
  };

  hosts =
    (lib.mapAttrsToList (dnsInfo "home" "lan") pd.net.lan.hosts)
    ++ (lib.mapAttrsToList (dnsInfo "wg" "wg") pd.net.wg.hosts);

  host-record =
    host:
    lib.concatStringsSep "," (
      [
        host.name
        host.ipv4
      ]
      ++ lib.optional (host.ipv6 != null) host.ipv6
    );

  cnames =
    let
      expand = cnames: lib.strings.concatStringsSep "," cnames;
      toEntry = host: lib.lists.optional (host.cnames != [ ]) "${expand host.cnames},${host.name}";
    in
    builtins.concatMap toEntry hosts;

in
{
  imports = [ ./monitoring.nix ];

  networking = {
    nameservers = lib.mkForce [
      "127.0.0.1"
      "::1"
    ];
    firewall = {
      allowedTCPPorts = [ 53 ];
      allowedUDPPorts = [ 53 ];
    };
  };

  services.dnsmasq = {
    enable = true;

    # we manage this directly ourselves instead; no need for nixos
    # to wire up a separate resolvconf, override nameservers, etc.
    resolveLocalQueries = false;

    # https://thekelleys.org.uk/dnsmasq/docs/dnsmasq-man.html
    settings = {
      port = 53;
      no-resolv = true;
      local = [
        "/home/"
        "/wg/"
      ];

      # default is 150
      cache-size = 2000;

      host-record = map host-record hosts;
      cname = cnames;

      strict-order = true;
      server = [
        "1.1.1.1"
        "8.8.8.8"
      ];

      no-hosts = true;
      expand-hosts = false;
      addn-hosts = [ "${pkgs.pd.dns-blocklist}/dns-blocklist" ];
    };
  };

}
