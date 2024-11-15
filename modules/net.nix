# home 192.168.1.0/22
# wg   192.168.20.0/24
#
# computers:
#   wrt:   .1, gw
#   rpt:   .2
#   desk:  .10
#   span:  .11
#   htpc:  .12
#   pi:    .13, wg
#   air:   .14
#
# phones:
#   pd:    .50
#   erin:  .51
#
# appliances:
#   nas:   .100
#   tv:    .101
#   do:    .110
#   hera:  .120
{ lib, ... }:
rec {
  lan = {
    cidr = "192.168.0.0/22";
    hosts = lib.filterAttrs (_: h: h ? "lan") hosts;
    ips = lib.mapAttrs (_: v: v.lan.ip) lan.hosts;
  };

  wg = {
    cidr = "192.168.20.0/24";
    hosts = lib.filterAttrs (_: h: h ? "wg") hosts;
    ips = lib.mapAttrs (_: v: v.wg.ip) wg.hosts;
    pks = lib.mapAttrs (_: v: v.wg.publicKey) wg.hosts;
  };

  # hosts with ssh enabled
  ssh.hosts = lib.filterAttrs (_: h: (h.ssh or { }) != false) hosts;

  hosts = {
    wrt = {
      lan.ip = "192.168.1.1";
      ssh.user = "root";
    };

    rpt = {
      lan.ip = "192.168.1.2";
      ssh.user = "root";
    };

    desk = {
      lan.ip = "192.168.1.10";
    };

    span = {
      lan.ip = "192.168.1.11";
      ssh.port = 22;
      wg = {
        ip = "192.168.20.11";
        publicKey = "ifiRznc81W75NIIq53+8BH6uJ3iJODbXdAk+ND1J+3U=";
      };
    };

    htpc = {
      lan.ip = "192.168.1.12";
      cnames = [
        "grafana"
        "jellyfin"
        "kodi"
        "prom"
        "store"
        "torrent"
        "www"
      ];
      wg = {
        ip = "192.168.20.12";
        publicKey = "sZql5WlnNt45LuiQUjow0y+Hc9LdWW7nnSUjOMHSsgw=";
      };
    };

    pi = {
      lan.ip = "192.168.1.13";
      cnames = [
        "ns"
        "wg"
      ];
      wg = {
        ip = "192.168.20.13";
        publicKey = "xDPPIIjA72BrCFC+5eqJn7IiC0xeI6Dof38Inj+tXwg=";
      };
    };

    air = {
      lan.ip = "192.168.1.14";
      ssh = false;
    };

    pdroid = {
      lan.ip = "192.168.1.50";
      ssh = false;
      wg = {
        ip = "192.168.20.50";
        publicKey = "YeemrKq8W+3LwT0Z4nqgivC/zGTKZdwQHL0d+W3lNTc=";
      };
    };

    nas = {
      lan.ip = "192.168.1.100";
    };

    tv = {
      lan.ip = "192.168.1.101";
      ssh = false;
    };

    donix = {
      ssh.hostname = "donix.krh.me";
      wg = {
        ip = "192.168.20.110";
        publicKey = "WZgf+DC6SBQeatqOgpC2j6tvIu5VxKi/WgdbIU/m7wg=";
      };
    };

    hera = {
      lan.ip = "192.168.1.120";
      ssh = false;
    };
  };
}
