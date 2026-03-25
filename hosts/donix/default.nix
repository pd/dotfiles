{ lib, ... }:
let
  friends = {
    rhys = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIOZGcHggrgVlMOSh2lG3i8Jp1vA2rz7NyuWSnlVYnUh" ];
    packrat386 = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKykCUxNhZg/CNBEKAb2LPtW4oD6oB1DEeAR55hslr5D" ];
  };

  mkAdmin = user: keys: {
    isNormalUser = true;
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
    ./gatus.nix
    ./infect.nix
    ./ntfy.nix
    ./httpd.nix
  ];

  system.stateVersion = "24.05";
  time.timeZone = "America/Chicago";
  docs.enable = false;

  nixpkgs.hostPlatform.system = "x86_64-linux";
  networking.hostName = "donix";
  networking.nameservers = [
    "1.1.1.1"
    "8.8.8.8"
  ];

  networking.useDHCP = false;
  networking.useNetworkd = true;
  wg.offLan = true;

  users.users = lib.mapAttrs mkAdmin friends;

  monitoring.heart.monitor = [
    "pi"
    "htpc"
  ];
  monitoring.processes = {
    caddy.comm = [ "caddy" ];
    gatus.comm = [ "gatus" ];
    ntfy.comm = [ "ntfy" ];
  };
}
