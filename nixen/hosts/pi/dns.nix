{ lib, ... }:
let
  net = import ../../modules/net.nix;

  onLan = lib.filterAttrs (_: v: v ? "lan") net.hosts;
  ips = lib.mapAttrsToList (k: v: {
    ip = v.lan.ip;
    names = [ k ] ++ (v.cnames or [ ]);
  }) onLan;

  toRecord = ip: n: {
    name = "${n}.home";
    value = ip;
  };
  records = builtins.concatMap (rcd: lib.map (toRecord rcd.ip) rcd.names) ips;
in
{
  networking.firewall = {
    allowedTCPPorts = [ 53 ];
    allowedUDPPorts = [ 53 ];
  };

  services.blocky = {
    enable = true;
    settings = {
      ports.dns = 53;
      ede.enable = true;

      log = {
        level = "info";
        format = "json";
        timestamp = true;
        privacy = false;
      };

      upstreams.groups.default = [
        "1.1.1.1"
        "1.0.0.1"
        "8.8.8.8"
        "https://one.one.one.one/dns-query"
      ];

      bootstrapDns = [
        {
          upstream = "https://one.one.one.one/dns-query";
          ips = [
            "1.1.1.1"
            "1.0.0.1"
          ];
        }
      ];

      blocking = {
        # TODO: becomes `denylists` in unstable
        blackLists = {
          ads = [ "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts" ];
        };

        clientGroupsBlock = {
          default = [ "ads" ];
        };
      };

      customDNS = {
        customTTL = "10m";
        filterUnmappedTypes = true;
        mapping = lib.listToAttrs records;
      };

      caching = {
        cacheTimeNegative = "30s";
      };
    };
  };
}
