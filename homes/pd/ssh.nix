{
  lib,
  hostname,
  pd,
  ...
}:
let
  others = removeAttrs pd.net.ssh.hosts [ hostname ];
  hostBlock =
    _: host:
    {
      HostName = host.ssh.hostname;
      Port = host.ssh.port;
    }
    // lib.optionalAttrs (host.ssh ? user) { User = host.ssh.user; };
in
{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;

    extraConfig = ''
      IdentitiesOnly yes
      IdentityFile ~/.ssh/id_ed25519
    '';

    settings = {
      "*" = {
        ForwardAgent = false;
        AddKeysToAgent = "no";
        Compression = false;
        ServerAliveInterval = 0;
        ServerAliveCountMax = 3;
        HashKnownHosts = false;
        UserKnownHostsFile = "~/.ssh/known_hosts";
        ControlMaster = "no";
        ControlPath = "~/.ssh/master-%r@%n:%p";
        ControlPersist = "no";
      };
    }
    // (lib.mapAttrs hostBlock others);
  };
}
