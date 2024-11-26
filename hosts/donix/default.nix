{ lib, ... }:
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

  users.users.rhys = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "networkmanager"
      "keys"
    ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIOZGcHggrgVlMOSh2lG3i8Jp1vA2rz7NyuWSnlVYnUh"
    ];
  };
}
