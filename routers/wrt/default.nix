{
  lib,
  net,
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
uci.mkRouter "wrt" packages {
  uci.retain = [
    "firewall" # TODO
    "luci"
    "rpcd"
    "ubootenv"
    "ucitrack"
    "uhttpd"
  ];

  uci.settings = {
    prometheus-node-exporter-lua = {
      prometheus-node-exporter-lua.main = {
        listen_interface = "lan";
        listen_ipv6 = true;
        listen_port = 9100;
      };
    };

    dhcp = import ./uci.dhcp.nix { inherit lib net; };
    ddns = import ./uci.ddns.nix { inherit lib; };
    network = import ./uci.network.nix { inherit net uci; };
    upnpd = import ./uci.upnpd.nix { };
    wireless = import ./uci.wireless.nix { };
  };
}
