{
  dmerge,
  lib,
  pd,
  pkgs,
  ...
}:
lib.uci.mkRouter "wrt" ./secrets.yaml {
  uci.retain = [
    "luci"
    "rpcd"
    "ubootenv"
  ];

  uci.settings = {
    dhcp = import ./uci.dhcp.nix { inherit lib pd; };
    ddns = import ./uci.ddns.nix { inherit lib; };
    firewall = import ./uci.firewall.nix { inherit dmerge pd; };
    network = import ./uci.network.nix { inherit lib pd; };
    uhttpd = import ./uci.uhttpd.nix { inherit lib pd; };
    upnpd = import ./uci.upnpd.nix { inherit pd; };
    wireless = import ./uci.wireless.nix { inherit lib; };
  };

  # deploySteps is marked internal but I want to copy a file
  # and this is the best I've got
  deploySteps.ddns =
    let
      path = "/usr/lib/ddns/update_wg_pi.sh";
      ddnsWG6 = pkgs.writeTextFile {
        name = "update_wg_pi.sh";
        text = builtins.readFile ./update_wg_pi.sh;
      };
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
