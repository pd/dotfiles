rec {
  workstations = {
    ssh = desk.ssh ++ armspan.ssh;
  };

  armspan = {
    ssh = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBLvrXCgikdSPUKxIoYJjiWFAWHW7AAmLZa9WuIXQ4Ub" ];
  };

  desk = {
    ssh = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFWXGORnABVvcG3aIp/l0Y0mK6puJHZndPkd+XeoyV5i" ];
  };

  pi = {
    root.ssh = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBnU71ehvOaoITSJPp+dlmmiZ21tZov5d1pWk/gjJPKF root@pi"
    ];
  };
}
