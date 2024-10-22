{
  lib,
  pkgs,
  config,
  ...
}:
let
  cnames = host: host.cnames or [];

  net = import ../../modules/net.nix;
  wanHosts = lib.filterAttrs (_: peer: peer ? "wg0") net.hosts;
  records = lib.flatten (
    lib.mapAttrsToList (
      name: host:
      [ ''"${name}.home. IN A ${host.wg0.ip}"'' ]
      ++ builtins.map (cname: ''"${cname}.home. IN CNAME ${name}.home."'') (cnames host)
    ) wanHosts
  );

in
{
  imports = [
    ../../modules/base.nix
    ../../modules/wg/server.nix
    ./dns.nix
    ./www.nix
    ./users/pd.nix
    ./users/rhys.nix
  ];

  system.stateVersion = "24.05";
  networking.hostName = "donix";
  time.timeZone = "America/Chicago";
}
