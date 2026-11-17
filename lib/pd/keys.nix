rec {
  workstations = {
    ssh = desk.pd.ssh ++ armspan.pd.ssh;
  };

  armspan = {
    host = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJH4jY022cUJqrgkhW1lnjbPaF6eqfizeKC9XJgzmWDz";
    pd.ssh = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBLvrXCgikdSPUKxIoYJjiWFAWHW7AAmLZa9WuIXQ4Ub" ];
  };

  desk = {
    host = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILcEEOBXFz8ou90PBcjGHCTFk7IId7f1yWos4wcMOw4O";
    pd.ssh = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFWXGORnABVvcG3aIp/l0Y0mK6puJHZndPkd+XeoyV5i" ];
  };

  pi = {
    host = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKkt8ZKVXnlhfBG2WhvVotx8mH8T1gq0Wc89UdcnB4Lv";
    root.ssh = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBnU71ehvOaoITSJPp+dlmmiZ21tZov5d1pWk/gjJPKF"
    ];
  };

  donix.host = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIdTBpuFgrGnXA734UeS+1eUVB4UwkOqyiRFIzc3hSqh";
  htpc.host = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILXXIw+W67xspYHKnGzwcRKnQD+kc2LxoOoqqW446peT";
  mini.host = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL3CTEUXX86VTMmQ5fD/9hxs3vMSMzZ9+9CZwM5Nmjzf";
  nas.host = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPCr0As2pRGeusU+VocQD8dsY8NMKze6O9Ox9TOjHMfC";
  rpt.host = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHXZ2e71rIHuRQfUSf7JfjNkpo1846fGQXzg6VdbiCUf";
  wrt.host = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICRtsDNp+fMVKf3lBPBOO0XyFDoARVJuHAtz0rkba164";

  yubis = {
    ring = "age1yubikey1qwpnm5gflajwaptyzmhrapandm2flm2x97hlsps9lt3q48zkrnqvcx4qk4g";
    cab = "age1yubikey1q2m5hrmgxhhg6yzdz4y8sstucwlaklrmghl4nx69qrtyknunsd8m6ukcs8q";
  };
}
