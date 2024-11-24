{
  net,
  pkgs,
}:
let
  uci = import ./uci.nix {
    inherit net;
    inherit (pkgs) lib;
  };

  mkRouter =
    name:
    import ./${name} {
      inherit
        net
        pkgs
        uci
        ;
      inherit (pkgs) lib;
    };
in
{
  openwrt.wrt = mkRouter "wrt";
  openwrt.rpt = mkRouter "rpt";
}
