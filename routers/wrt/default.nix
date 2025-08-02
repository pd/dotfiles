{
  dmerge,
  lib,
  pd,
  uci,
  ...
}:
uci.mkRouter "wrt" ./secrets.yaml {
  uci.retain = [
    "luci"
    "rpcd"
    "ubootenv"
  ];

  uci.settings = {
    dhcp = import ./uci.dhcp.nix { inherit lib pd; };
    ddns = import ./uci.ddns.nix { inherit lib; };
    firewall = import ./uci.firewall.nix { inherit dmerge pd; };
    network = import ./uci.network.nix { inherit pd uci; };
    uhttpd = import ./uci.uhttpd.nix { inherit lib pd; };
    upnpd = import ./uci.upnpd.nix { inherit pd; };
    wireless = import ./uci.wireless.nix { inherit uci; };
  };

  # TODO: should not be touching deploySteps
  deploySteps.ddns =
    let
      path = "/usr/lib/ddns/update_wg_pi.sh";
    in
    {
      priority = 100;
      copy = ''
        scp ${./update_wg_pi.sh} device:${path}
      '';
      apply = ''
        chown root:root ${path}
        chmod 544 ${path}
      '';
    };
}
