{
  dmerge,
  lib,
  pd,
  pkgs,
  uci,
  ...
}:
let
  ddnsWG6 = pkgs.writeTextFile {
    name = "update_wg_pi.sh";
    text = builtins.readFile ./update_wg_pi.sh;
  };
in
uci.mkRouter "wrt" ./secrets.yaml {
  uci.retain = [
    "luci"
    "rpcd"
    "ubootenv"
    "uhttpd"
  ];

  uci.settings = {
    dhcp = import ./uci.dhcp.nix { inherit lib pd; };
    ddns = import ./uci.ddns.nix { inherit lib; };
    firewall = import ./uci.firewall.nix { inherit dmerge pd uci; };
    network = import ./uci.network.nix { inherit pd uci; };
    upnpd = import ./uci.upnpd.nix { inherit pd; };
    wireless = import ./uci.wireless.nix { inherit pd uci; };
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
