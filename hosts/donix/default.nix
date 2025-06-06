{ lib, ... }:
let
  friends = {
    rhys = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIOZGcHggrgVlMOSh2lG3i8Jp1vA2rz7NyuWSnlVYnUh" ];
    packrat386 = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKykCUxNhZg/CNBEKAb2LPtW4oD6oB1DEeAR55hslr5D" ];
  };

  mkAdmin = user: keys: {
    isNormalUsers = true;
    extraGroups = [
      "wheel"
      "networkmanager"
      "keys"
    ];
    openssh.authorizedKeys.keys = keys;
  };
in
{
  imports = [
    ../../modules/core
    ./infect.nix
    ./www.nix
  ];

  system.stateVersion = "24.05";
  networking.hostName = "donix";
  networking.nameservers = [
    "1.1.1.1"
    "8.8.8.8"
  ];
  wg.offLan = true;

  time.timeZone = "America/Chicago";

  users.users = lib.mapAttrs mkAdmin friends;
}
