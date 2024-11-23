{
  pkgs,
  lib,
  net,
  ...
}:
let
  # doesn't use go.mod so just build it the old fashioned way
  hosts-bl = pkgs.stdenv.mkDerivation {
    name = "hosts-bl";
    src = pkgs.fetchFromGitHub {
      owner = "ScriptTiger";
      repo = "Hosts-BL";
      rev = "b3ac0a50fce8e714e754a17e6a11f8709386782c";
      hash = "sha256-w+4dEWwFMjBbeJPOqMrzLBBzPYh/V5SfV2BMrI0p3nw=";
    };

    configurePhase = ''
      export GOCACHE=$TMPDIR/go-cache
      export CGO_ENABLED=0
    '';

    installPhase = ''
      mkdir -p $out/bin
      ${pkgs.unstable.go}/bin/go build -o $out/bin/hosts-bl $src/hosts-bl.go $src/include_other.go
    '';
  };

  blockLists = pkgs.stdenv.mkDerivation {
    name = "dns-block-lists";
    src = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/StevenBlack/hosts/b307ce09a15cbe7773a65411f75c7d4fd403a230/hosts";
      hash = "sha256-oCej3Kti/Iqim/sq4BA9z4Yr/IGLtdwXTiieQe8f6Uo=";
    };

    dontUnpack = true;
    installPhase = ''
      mkdir $out
      ${hosts-bl}/bin/hosts-bl -f ipv6 -to_blackhole_v6 ::0 -i $src -o $out/dns-block-list
    '';
  };

  dnsInfo =
    tld: net: name: host:
    let
      ipv4 = host."${net}".ip;
      tail = lib.drop 2 (lib.splitString "." ipv4);
      netnum = lib.head tail;
      hostnum = lib.last tail;
      ipv6 = "fded:${netnum}::${hostnum}";
    in
    {
      inherit ipv4 ipv6;
      name = "${name}.${tld}";
      cnames = map (n: "${n}.${tld}") (host.cnames or [ ]);
    };

  hosts =
    (lib.mapAttrsToList (dnsInfo "home" "lan") net.lan.hosts)
    ++ (lib.mapAttrsToList (dnsInfo "wg" "wg") net.wg.hosts);

  host-records = map (host: "${host.name},${host.ipv4},${host.ipv6}") hosts;
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
