{
  pkgs,
  lib,
  net,
  ...
}:
let
  blockLists = pkgs.stdenv.mkDerivation {
    name = "dns-block-lists";
    src = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/StevenBlack/hosts/b307ce09a15cbe7773a65411f75c7d4fd403a230/hosts";
      hash = "sha256-oCej3Kti/Iqim/sq4BA9z4Yr/IGLtdwXTiieQe8f6Uo=";
    };

    dontUnpack = true;
    installPhase = ''
      mkdir $out
      grep '^0.0.0.0' $src > $out/dns-block-list
    '';
  };

  dnsInfo = tld: net: name: host: {
    name = "${name}.${tld}";
    ip = host."${net}".ip;
    cnames = map (n: "${n}.${tld}") (host.cnames or [ ]);
  };

  hosts =
    (lib.mapAttrsToList (dnsInfo "home" "lan") net.lan.hosts)
    ++ (lib.mapAttrsToList (dnsInfo "wg" "wg") net.wg.hosts);

  host-records = map (host: "${host.name},${host.ip}") hosts;
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

    # https://thekelleys.org.uk/dnsmasq/docs/dnsmasq-man.html
    settings = {
      port = 53;
      no-resolv = true;
      local = [
        "/home/"
        "/wg/"
      ];

      host-record = host-records;
      cname = cnames;

      strict-order = true;
      server = [
        "1.1.1.1"
        "8.8.8.8"
      ];

      no-hosts = true;
      expand-hosts = false;
      addn-hosts = [ "${blockLists}/dns-block-list" ];
    };
  };

}
