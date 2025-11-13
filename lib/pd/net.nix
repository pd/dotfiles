# home 192.168.40.0/22 fded:40::/48
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
#   win:        .15
#   armspan:    .16
#
# mobile:
#   pd:         .50
#   erin:       .51
#   ipad:       .52
#   avp:        .53
#
# extra interfaces:
#   desk-wifi:  .80
#   htpc-wifi:  .82
#   win-wifi:   .85
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

  mkPub =
    id: lan:
    mkIf "pub" (lan != false) { } {
      # AT&T assigns a /60 or something, then delegates 1e2f::/64. It's
      # not technically valid to re-delegate that /64, but external IPs
      # for LAN hosts all land within it. Something like that, who
      # knows. Mostly works AFAICT.
      ipv6 = "2600:1700:3040:1e2f::${toString id}";
    };

  mkLan =
    id: lan: v6:
    mkIf "lan" (lan != false) { } (
      {
        ipv4 = "192.168.40.${toString id}";
      }
      // (if v6 then { ipv6 = "fded:40::${toString id}"; } else { })
    );

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
      v6 ? true,
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
        v6
        ;
    }
    // (mkLan id lan v6)
    // (mkPub id lan)
    // (mkWg id wg)
    // (mkSsh hostname ssh);
in
rec {
  wifi = {
    ssid = "bazqux";
  };

  lan = {
    cidr = "192.168.40.0/22";
    cidr6 = "fded:40::/64";
    hosts = filterAttrs (_: h: h ? lan) hosts;
    ipv4 = mapAttrs (_: v: v.lan.ipv4) lan.hosts;
    ipv6 = mapAttrs (_: v: v.lan.ipv6) (filterAttrs (_: h: h.v6) lan.hosts);
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
      macs = [
        "94:83:c4:a3:31:20" # eth1
        "94:83:c4:a3:31:22" # eth0, lan1..5, br-lan
        "94:83:c4:a3:31:23" # phy0-ap0
        "94:83:c4:a3:31:24" # phy1-ap0
      ];
    };

    rpt = {
      id = 2;
      duid = "000300019483c4a4aad2";
      macs = [
        "94:83:c4:a4:aa:d2" # eth0
        "94:83:c4:a4:aa:d4" # lan1, lan2, br-lan
        "94:83:c4:a4:aa:d6" # phy1-ap0
        "96:83:c4:a4:aa:d6" # phy1-sta0
      ];
      ssh.user = "root";
    };

    desk = {
      id = 10;
      duid = "00020000ab1128098d9beaf51da5";
      macs = [ "2c:f0:5d:db:8d:f3" ];
    };

    desk-wifi = {
      id = 80;
      duid = "0002deadc0decafe"; # overridden in systemd-networkd config
      macs = [ "14:cc:20:23:ea:fc" ];
    };

    span = {
      id = 11;
      duid = "00010001294a1d92f8ffc2698bb6";
      macs = [ "f8:ff:c2:69:8b:b6" ];
      ssh.port = 22;
      wg.publicKey = "ifiRznc81W75NIIq53+8BH6uJ3iJODbXdAk+ND1J+3U=";
      cnames = [ "orb" ];
    };

    htpc = {
      id = 12;
      duid = "00020000ab11c0d46c058a8aa1e2";
      macs = [ "1c:69:7a:a2:40:de" ];
      wg.publicKey = "sZql5WlnNt45LuiQUjow0y+Hc9LdWW7nnSUjOMHSsgw=";
      cnames = [
        "alerts"
        "grafana"
        "img"
        "jellyfin"
        "koito"
        "ns2"
        "prom"
        "scrobbler"
        "store"
        "torrent"
        "www"
      ];
    };

    htpc-wifi = {
      id = 82;
      duid = "0002deadc0decafe0082";
      macs = [ "b0:a4:60:17:89:87" ];
    };

    pi = {
      id = 13;
      duid = "00020000ab117bab113ac86ee6d1";
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

    win = {
      id = 15;
      duid = "00010001282008632cf05ddb8d13";
      macs = [ "2c:f0:5d:db:8d:13" ];
      ssh = false;
    };

    armspan = {
      id = 16;
      duid = "0001000130a80175842f57ce34ce";
      macs = [ "c6:d3:86:c1:3d:e0" ];
      ssh.port = 22;
      wg.publicKey = "6dbFtf4/jeF7/H4UDEnxkFbTSmsaXs43msfJ6YcydTk=";
    };

    win-wifi = {
      id = 85;
      duid = "00010001282008632cf05ddb8d13";
      macs = [ "14:cc:20:23:ea:6c" ];
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

    avp = {
      id = 53;
      macs = [ "ee:47:b2:66:96:8e" ];
      ssh = false;
    };

    nas = {
      id = 100;
      duid = "000300019009d05929cc";
      macs = [ "90:09:d0:59:29:cc" ];

      # synology ipv6 impl is just hosed, it never picks up the ULA no
      # matter what I do, and if I instead assign it manually nothing
      # is routable.
      v6 = false;
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
