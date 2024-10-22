{
  lib,
  ...
}:
let
  net = import ../../modules/net.nix;
  wanHosts = lib.filterAttrs (_: peer: peer ? "wg0") net.hosts;
  records = lib.flatten (
    lib.mapAttrsToList (
      name: host:
      [ ''"${name}.home. IN A ${host.wg0.ip}"'' ]
      ++ builtins.map (cname: ''"${cname}.home. IN CNAME ${name}.home."'') (host.cnames or [])
    ) wanHosts
  );
in
{
  networking.firewall = {
    allowedTCPPorts = [ 53 ];
    allowedUDPPorts = [ 53 ];
  };

  services.unbound = {
    enable = true;
    settings = {
      server = {
        interface = [
          "0.0.0.0"
          "::1"
        ];
        access-control = [
          "127.0.0.1/8 allow"
          "10.100.100.0/24 allow"
        ];

        local-zone = [ "home. static" ];
        local-data = records;
      };

      forward-zone = [
        {
          name = ".";
          forward-addr = [
            "1.1.1.1"
            "8.8.8.8"
          ];
        }
      ];
    };
  };
}
