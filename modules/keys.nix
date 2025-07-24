rec {
  workstations = {
    ssh = desk.ssh ++ span.ssh;
  };

  desk = {
    ssh = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFWXGORnABVvcG3aIp/l0Y0mK6puJHZndPkd+XeoyV5i" ];
  };

  span = {
    ssh = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINL/LBITUZGTMudEJ46NLPy/MbtuRfc9dPETK+d9KT5F"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCgooJHfce7vOjmFCyWF/qqepTsQ1ohmESASroppNy4RiKHRVBWNqnMJDlSXfGlKOxVrnIMxcHAP9jbNhYT/x7/l8CBZvIngmNM2gcrFTwq5EF8QvTKc6Sp1dLJvOVXsmhJ2QmwJhzv2Q5LThKT8HMX3k9QAihppGjxAiFRZOGrmNhlIoEmRSRo3bjK5m3Z57WL640jLiYqzwG3YtLErToHCuMMgslkZsdPGoOnzX8bBpr/O1bYSqEDCm43p96AIf9KSeemZhztA/Kn/Cyjsj0d8egjJlLTbpFPR+3yoAT6YYO/bUmyYomZ/H29Dy2O//pgDCV6LlnARK8HlcZxRyid"
    ];
  };
}
