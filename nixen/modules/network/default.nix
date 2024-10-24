{ lib, ... }:
{
  imports = [
    ./lan.nix
    ./wan.nix
  ];

  lan.enable = lib.mkDefault true;
  wan.enable = lib.mkDefault false;
}
