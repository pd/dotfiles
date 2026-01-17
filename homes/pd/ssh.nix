{
  lib,
  hostname,
  pd,
  ...
}:
let
  others = removeAttrs pd.net.ssh.hosts [ hostname ];
  matchBlock = _: host: host.ssh;
in
{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;

    extraConfig = ''
      IdentitiesOnly yes
      IdentityFile ~/.ssh/id_ed25519
    '';

    matchBlocks = {
      "*" = {
        forwardAgent = false;
        addKeysToAgent = "no";
        compression = false;
        serverAliveInterval = 0;
        serverAliveCountMax = 3;
        hashKnownHosts = false;
        userKnownHostsFile = "~/.ssh/known_hosts";
        controlMaster = "no";
        controlPath = "~/.ssh/master-%r@%n:%p";
        controlPersist = "no";
      };
    }
    // (lib.mapAttrs matchBlock others);
  };
}
