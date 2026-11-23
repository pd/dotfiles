{ pd, pkgs, ... }:
let
  mini = pd.net.hosts.mini.ssh;
in
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nameserver
  ];

  system.stateVersion = "26.05";
  time.timeZone = "America/Chicago";
  docs.enable = false;

  networking.hostName = "pi";
  lan.wired.interface = "end0";
  wg.natInterface = "end0";

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  environment.systemPackages = with pkgs; [
    nmap
    traceroute
  ];

  monitoring.heartbeat = true;
  monitoring.processes = {
    dnsmasq.comm = [ "dnsmasq" ];
  };

  programs.ssh.extraConfig = ''
    Host mini-builder
      HostName ${mini.hostname}
      Port 31022
      HostKeyAlias mini-builder
      ConnectTimeout 5
  '';

  programs.ssh.knownHosts.mini-builder = {
    hostNames = [ "mini-builder" ];
    publicKeyFile = "${pkgs.path}/nixos/modules/profiles/keys/ssh_host_ed25519_key.pub";
  };

  nix = {
    distributedBuilds = true;
    buildMachines = [
      {
        hostName = "mini-builder";
        protocol = "ssh-ng";
        sshUser = "builder";
        sshKey = "/root/.ssh/id_ed25519";
        systems = [ "aarch64-linux" ];
        maxJobs = 8;
        speedFactor = 10;
        supportedFeatures = [
          "big-parallel"
          "benchmark"
        ];
      }
    ];
    settings.builders-use-substitutes = true;
  };
}
