{
  dmerge,
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

  ddnsWG6 = pkgs.writeTextFile {
    name = "update_wg_pi.sh";
    text = builtins.readFile ./update_wg_pi.sh;
  };
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

  # deploySteps is marked internal but I want to copy a file
  # and this is the best I've got
  deploySteps.ddns =
    let
      path = "/usr/lib/ddns/update_wg_pi.sh";
    in
    {
      priority = 100;
      copy = ''
        scp ${ddnsWG6} device:${path}
      '';
      apply = ''
        chown root:root ${path}
        chmod 544 ${path}
      '';
    };
}
