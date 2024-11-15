{
  lib,
  config,
  net,
  ...
}:
let
  host = net.hosts."${config.networking.hostName}";
in
{
  imports = [
    ./lan.nix
    ./wg.nix
  ];

  lan.enable = lib.mkDefault (host ? "lan");
  lan.ipv4 = lib.mkIf config.lan.enable host.lan.ip;

  wg.enable = lib.mkDefault (host ? "wg");
  wg.ipv4 = lib.mkIf config.wg.enable host.wg.ip;
  wg.publicKey = lib.mkIf config.wg.enable host.wg.publicKey;
}
