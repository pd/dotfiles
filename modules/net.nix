# home 192.168.40.0/22 fded:20::/48
# wg   192.168.60.0/24 fded:60::/48
#
# computers:
#   wrt:        .1, gw
#   rpt:        .2
#   desk:       .10
#   span:       .11
#   htpc:       .12
#   pi:         .13, wg
#   air:        .14
#   desk-wifi:  .20
#
# mobile:
#   pd:         .50
#   erin:       .51
#   ipad:       .52
#   avp:        .53
#
# appliances:
#   nas:        .100
#   tv:         .101
#   do:         .110
#   hera:       .120
#
# unknown:      .42/24
{ lib, ... }:
with lib;
let
  mkIf =
    k: on: custom: defaults:
    if on then { ${k} = defaults // custom; } else { };

  mkLan =
    id: lan:
    mkIf "lan" (lan != false) { } {
      ipv4 = "192.168.1.${toString id}";
      ipv6 = "fded:40::${toString id}";
    };

  mkWg =
    id: wg:
    mkIf "wg" (wg ? publicKey) wg {
      ipv4 = "192.168.60.${toString id}";
      ipv6 = "fded:60::${toString id}";
    };

  mkSsh =
    hostname: ssh:
    mkIf "ssh" (ssh != false) ssh {
      hostname = "${hostname}.home";
      port = 1222;
    };

  mkHost =
    hostname:
    _@{
      id,
      duid ? null,
      lan ? { },
      macs ? [ ],
      ssh ? { },
      wg ? { },
      cnames ? [ ],
    }:
    {
      inherit
        id
        duid
        macs
        cnames
        ;
    }
    // (mkLan id lan)
    // (mkWg id wg)
    // (mkSsh hostname ssh);
in
rec {
  lan = {
    cidr = "192.168.0.0/22";
    cidr6 = "fded:40::/64";
    hosts = filterAttrs (_: h: h ? lan) hosts;
    ipv4 = mapAttrs (_: v: v.lan.ipv4) lan.hosts;
    ipv6 = mapAttrs (_: v: v.lan.ipv6) lan.hosts;
  };

  wg = {
    cidr = "192.168.60.0/24";
    cidr6 = "fded:60::/64";
    hosts = filterAttrs (_: h: h ? wg) hosts;
    ipv4 = mapAttrs (_: v: v.wg.ipv4) wg.hosts;
    ipv6 = mapAttrs (_: v: v.wg.ipv6) wg.hosts;
    pks = mapAttrs (_: v: v.wg.publicKey) wg.hosts;
  };

  ssh.hosts = filterAttrs (_: h: h ? ssh) hosts;

  hosts = mapAttrs mkHost {
    wrt = {
      id = 1;
      ssh.user = "root";
      cnames = [ "gw" ];
    };

    rpt = {
      id = 2;
      duid = "000300019483c4a4aad4";
      macs = [ "94:83:c4:a4:aa:d4" ];
      ssh.user = "root";
    };

    desk = {
      id = 10;
      # TODO: support multiple DUIDs so lease matches across
      # nixos and windows
      duid = "000100012ecf7ce92cf05ddb8d13";
      macs = [
        "14:cc:20:23:ea:6c" # wlp0s20f3
        "2c:f0:5d:db:8d:13" # enp42s0
      ];
    };

    span = {
      id = 11;
      duid = "00010001294a1d92f8ffc2698bb6";
      macs = [ "f8:ff:c2:69:8b:b6" ];
      ssh.port = 22;
      wg.publicKey = "ifiRznc81W75NIIq53+8BH6uJ3iJODbXdAk+ND1J+3U=";
    };

    htpc = {
      id = 12;
      duid = "0004dd5908ad4d02b46426ad2e2b179289d9";
      macs = [ "b0:a4:60:17:89:87" ];
      wg.publicKey = "sZql5WlnNt45LuiQUjow0y+Hc9LdWW7nnSUjOMHSsgw=";
      cnames = [
        "grafana"
        "jellyfin"
        "ns2"
        "prom"
        "store"
        "torrent"
        "www"
      ];
    };

    pi = {
      id = 13;
      duid = "000103042eaecf0e000000000000";
      macs = [ "d8:3a:dd:70:a7:f5" ];
      wg.publicKey = "xDPPIIjA72BrCFC+5eqJn7IiC0xeI6Dof38Inj+tXwg=";
      cnames = [
        "ns"
        "ns1"
        "wg"
      ];
    };

    air = {
      id = 14;
      duid = "000100012c2b9b1910b58855b7af";
      macs = [ "10:b5:88:55:b7:af" ];
      ssh = false;
    };

    pdroid = {
      id = 50;
      macs = [ "d4:3a:2c:55:0a:dd" ];
      ssh = false;
      wg.publicKey = "YeemrKq8W+3LwT0Z4nqgivC/zGTKZdwQHL0d+W3lNTc=";
    };

    erphone = {
      id = 51;
      duid = "0001000129ad30d098502e23cf69";
      macs = [ "98:50:2e:23:cf:69" ];
      ssh = false;
    };

    erpad = {
      id = 52;
      duid = "000100012228a0c0f0766f681fad";
      macs = [ "f0:76:6f:68:1f:ad" ];
      ssh = false;
    };

    # avp = {
    #   id = 53;
    #   ssh = false;
    # };

    nas = {
      id = 100;
      duid = "000300019009d05929cc";
      macs = [ "90:09:d0:59:29:cc" ];
    };

    tv = {
      id = 101;
      macs = [ "60:8d:26:68:54:0c" ];
      ssh = false;
    };

    donix = {
      id = 110;
      lan = false;
      ssh.hostname = "donix.krh.me";
      wg.publicKey = "WZgf+DC6SBQeatqOgpC2j6tvIu5VxKi/WgdbIU/m7wg=";
    };

    hera = {
      id = 120;
      macs = [ "00:11:32:41:42:23" ];
      ssh = false;
    };
  };
}
