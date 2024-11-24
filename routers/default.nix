{
  dmerge,
  net,
  pkgs,
}:
let
  uci = import ./uci.nix {
    inherit dmerge net;
    inherit (pkgs) lib;
  };

  router =
    name:
    import ./${name} {
      inherit
        dmerge
        net
        pkgs
        uci
        ;
      inherit (pkgs) lib;
    };
in
{
  openwrt = pkgs.lib.genAttrs [
    "wrt"
    "rpt"
  ] router;
}
