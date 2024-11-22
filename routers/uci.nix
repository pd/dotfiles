{ lib, ... }:
with lib;
let
  keys = import ../modules/keys.nix;
  authorized-keys = concatStringsSep "\n" (keys.desk.ssh ++ keys.span.ssh);
in
{
  inherit authorized-keys;

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
}
