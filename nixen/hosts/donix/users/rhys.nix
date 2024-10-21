{ ... }:
{
  users.users = {
    rhys = {
      isNormalUser = true;
      extraGroups = [
        "networkmanager"
        "wheel"
        "keys"
      ];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIOZGcHggrgVlMOSh2lG3i8Jp1vA2rz7NyuWSnlVYnUh"
      ];
    };
  };
}
