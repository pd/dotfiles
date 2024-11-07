{ lib, net, ... }:
{
  imports = [
    ../../modules/core
    ./www.nix
  ];

  system.stateVersion = "24.05";

  networking.hostName = "donix";
  wan.natInterface = "ens3";

  networking.nameservers = lib.mkForce [
    net.hosts.pi.wg.ip
    "1.1.1.1"
    "8.8.8.8"
  ];

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