# home 192.168.1.0/24,  gw .254
# wg   10.100.100.0/24, gw .1
#
# computers:
#   do:    .1, wan only
#   desk:  .10
#   span:  .11
#   htpc:  .12
#   pi:    .13
#   air:   .14
#
# phones:
#   pd:    .50
#   erin:  .51
#
# appliances:
#   nas:   .100
#   tv:    .101
#   hera:  .120
{ lib, ... }:
rec {
  # hosts on the lan
  lan = {
    hosts = lib.filterAttrs (_: h: h ? "lan") hosts;
    ips = lib.mapAttrs (_: v: v.lan.ip) lan.hosts;
  };

  # hosts on the wg network
  wg = {
    hosts = lib.filterAttrs (_: h: h ? "wg") hosts;
    ips = lib.mapAttrs (_: v: v.wg.ip) wg.hosts;
    pks = lib.mapAttrs (_: v: v.wg.publicKey) wg.hosts;
  };

  # hosts with ssh enabled
  ssh.hosts = lib.filterAttrs (_: h: (h.ssh or { }) != false) hosts;

  hosts = {
    donix = {
      cnames = [ "srv" ];

      ssh = {
        hostname = "donix.krh.me";
      };

      wg = {
        ip = "10.100.100.1";
        publicKey = "WZgf+DC6SBQeatqOgpC2j6tvIu5VxKi/WgdbIU/m7wg=";
      };
    };

    wrt = {
      ssh = {
        user = "root";
      };

      lan = {
        ip = "192.168.1.1";
      };
    };

    desk = {
      lan = {
        ip = "192.168.1.10";
      };

      wg = {
        ip = "10.100.100.10";
        publicKey = "CA7HMcgdgxixmpikVA15Bxydi7pFriBbR3A3W7mE2BU=";
      };
    };

    span = {
      ssh = {
        port = 22;
      };

      lan = {
        ip = "192.168.1.11";
      };

      wg = {
        ip = "10.100.100.11";
        publicKey = "ifiRznc81W75NIIq53+8BH6uJ3iJODbXdAk+ND1J+3U=";
      };
    };

    htpc = {
      cnames = [
        "grafana"
        "jellyfin"
        "kodi"
        "prom"
        "store"
        "torrent"
        "www"
      ];

      lan = {
        ip = "192.168.1.12";
      };

      wg = {
        ip = "10.100.100.12";
        publicKey = "sZql5WlnNt45LuiQUjow0y+Hc9LdWW7nnSUjOMHSsgw=";
      };
    };

    pi = {
      cnames = [ "ns" ];

      lan = {
        ip = "192.168.1.13";
      };

      wg = {
        ip = "10.100.100.13";
        publicKey = "xDPPIIjA72BrCFC+5eqJn7IiC0xeI6Dof38Inj+tXwg=";
      };
    };

    erin = {
      ssh = false;

      lan = {
        ip = "192.168.1.14";
      };
    };

    nas = {
      lan = {
        ip = "192.168.1.100";
      };

      wg = {
        ip = "10.100.100.100";
        publicKey = "OgjpXp3AvhTRswErbC32X6zrnfEZeM3B/tjTPr87oig=";
      };
    };

    tv = {
      ssh = false;

      lan = {
        ip = "192.168.1.101";
      };
    };

    hera = {
      ssh = false;

      lan = {
        ip = "192.168.1.120";
      };
    };
  };
}