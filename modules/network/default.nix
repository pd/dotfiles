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
    ./wan.nix
  ];

  lan.enable = lib.mkDefault (host ? "lan");
  lan.ipv4 = lib.mkIf config.lan.enable host.lan.ip;

  wan.enable = lib.mkDefault (host ? "wg");
  wan.ipv4 = lib.mkIf config.wan.enable host.wg.ip;
  wan.publicKey = lib.mkIf config.wan.enable host.wg.publicKey;
}
