{ lib, net, ... }:
with lib;
let
  keys = import ../modules/keys.nix;
  authorized-keys = concatStringsSep "\n" (keys.desk.ssh ++ keys.span.ssh);
in
{
  bridgeLan =
    n: macaddr:
    let
      devs = map (i: {
        inherit macaddr;
        name = "lan${toString i}";
      }) (range 1 n);
    in
    devs
    ++ [
      {
        name = "br-lan";
        type = "bridge";
        ports = catAttrs "name" devs;
      }
    ];

  mkRouter =
    hostname: packages: custom:
    recursiveUpdate {
      inherit packages;

      deploy = {
        host = "${hostname}.home";
        sshConfig.Port = 1222;
      };

      users.root.hashedPassword = "$6$VaxrusIClFD3RwYc$GP9rU3UrVrn5Qz1PrtN716jWEAeYte1Lj6eq.NcY1iupk0f35P8MeiRhe7L0EkVrxNC0OT2Uah1VzJBwdJJav1";
      etc."dropbear/authorized_keys".text = authorized-keys;

      uci.sopsSecrets = ./${hostname}/secrets.yaml;

      uci.settings = {
        dropbear.dropbear = [
          {
            Interface = "lan";
            Port = 1222;
            PasswordAuth = "off";
          }
        ];

        system = {
          system = [
            {
              inherit hostname;
              zonename = "America/Chicago";
            }
          ];

          timeserver.ntp = {
            enabled = true;
            enable_server = false;
            server = [
              "0.openwrt.pool.ntp.org"
              "1.openwrt.pool.ntp.org"
              "2.openwrt.pool.ntp.org"
              "3.openwrt.pool.ntp.org"
            ];
          };
        };

        network = {
          interface.loopback = {
            device = "lo";
            proto = "static";
            ipaddr = "127.0.0.1";
            netmask = "255.0.0.0";
          };

          interface.lan = {
            device = "br-lan";
            proto = "static";
            netmask = "255.255.252.0";
            dns = [
              net.lan.ipv6.pi
              net.lan.ipv4.pi
              net.lan.ipv6.htpc
              net.lan.ipv4.htpc
            ];
            dns_search = [ "home" ];
          };
        };
      };

    } custom;
}
