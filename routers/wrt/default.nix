{
  lib,
  net,
  pkgs,
  uci,
  ...
}:
let
  packages = [
    "bind-dig"
    "ddns-scripts-digitalocean"
    "luci-app-ddns"
    "luci-app-upnp"
    "prometheus-node-exporter-lua"
    "prometheus-node-exporter-lua-nat_traffic"
    "prometheus-node-exporter-lua-netstat"
    "prometheus-node-exporter-lua-openwrt"
    "prometheus-node-exporter-lua-wifi"
    "prometheus-node-exporter-lua-wifi_stations"
    "tcpdump"
  ];
in
{
  inherit packages;
  deploy.host = "wrt.home";
  deploy.sshConfig = {
    Port = 1222;
  };

  users.root.hashedPassword = "$6$VaxrusIClFD3RwYc$GP9rU3UrVrn5Qz1PrtN716jWEAeYte1Lj6eq.NcY1iupk0f35P8MeiRhe7L0EkVrxNC0OT2Uah1VzJBwdJJav1";
  etc."dropbear/authorized_keys".text = uci.authorized-keys;

  uci.retain = [
    "firewall" # TODO
    "luci"
    "rpcd"
    "ubootenv"
    "ucitrack"
    "uhttpd"
  ];

  uci.sopsSecrets = ./secrets.yaml;
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
          hostname = "wrt";
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

    prometheus-node-exporter-lua = {
      prometheus-node-exporter-lua.main = {
        listen_interface = "lan";
        listen_ipv6 = true;
        listen_port = 9100;
      };
    };

    dhcp = import ./uci.dhcp.nix { inherit lib; };
    ddns = import ./uci.ddns.nix { inherit lib; };
    network = import ./uci.network.nix { inherit uci; };
    upnpd = import ./uci.upnpd.nix { };
    wireless = import ./uci.wireless.nix { };
  };
}
