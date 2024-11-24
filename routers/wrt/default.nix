{
  dmerge,
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
    "luci"
    "rpcd"
    "ubootenv"
    "ucitrack"
    "uhttpd"
  ];

  uci.settings = {
    dhcp = import ./uci.dhcp.nix { inherit lib net; };
    ddns = import ./uci.ddns.nix { inherit lib; };
    firewall = import ./uci.firewall.nix { inherit dmerge net uci; };
    network = import ./uci.network.nix { inherit net uci; };
    upnpd = import ./uci.upnpd.nix { };
    wireless = import ./uci.wireless.nix { };

    prometheus-node-exporter-lua = {
      prometheus-node-exporter-lua.main = {
        listen_interface = "lan";
        listen_ipv6 = true;
        listen_port = 9100;
      };
    };
  };
}
