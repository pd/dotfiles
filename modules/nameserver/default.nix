{
  pkgs,
  lib,
  net,
  ...
}:
let
  # performing the relevant bits of: https://github.com/ScriptTiger/Hosts-BL
  block-lists = pkgs.runCommand "block-lists" { } ''
    mkdir $out
    cat ${pkgs.unstable.stevenblack-blocklist}/hosts |
      awk '/^0.0.0.0 [a-z]/ { print $2 }' |
      pr -9 -t -T -a -s' ' - |
      awk '{ print "0.0.0.0 " $0; print "::0 " $0 }' > $out/dns-block-list
  '';

  dnsInfo = tld: net: name: host: {
    inherit (host."${net}") ipv4;
    name = "${name}.${tld}";
    cnames = map (n: "${n}.${tld}") (host.cnames or [ ]);
    ipv6 = host.ipv6 or false;
  };

  hosts =
    (lib.mapAttrsToList (dnsInfo "home" "lan") net.lan.hosts)
    ++ (lib.mapAttrsToList (dnsInfo "wg" "wg") net.wg.hosts);

  host-record =
    host:
    if host.ipv6 != false then
      "${host.name},${host.ipv4},${host.ipv6}"
    else
      "${host.name},${host.ipv4}";
  cnames =
    let
      expand = cnames: lib.strings.concatStringsSep "," cnames;
      toEntry = host: lib.lists.optional (host.cnames != [ ]) "${expand host.cnames},${host.name}";
    in
    builtins.concatMap toEntry hosts;

in
{
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

      host-record = map host-record hosts;
      cname = cnames;

      strict-order = true;
      server = [
        "1.1.1.1"
        "8.8.8.8"
      ];

      no-hosts = true;
      expand-hosts = false;
      addn-hosts = [ "${block-lists}/dns-block-list" ];
    };
  };

}
