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

  block-lists = pkgs.runCommand "block-lists" { } ''
    mkdir $out
    ${hosts-bl}/bin/hosts-bl \
      -f ipv6 \
      -to_blackhole_v6 ::0 \
      -i ${pkgs.unstable.stevenblack-blocklist}/hosts \
      -o $out/dns-block-list
  '';

  dnsInfo = tld: net: name: host: {
    inherit (host."${net}") ipv4 ipv6;
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
      addn-hosts = [ "${block-lists}/dns-block-list" ];
    };
  };

}
