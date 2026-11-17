{
  config,
  lib,
  hostname,
  pd,
  ...
}:
let
  others = removeAttrs pd.net.ssh.hosts [ hostname ];
  hostBlock =
    name: host:
    {
      HostName = host.ssh.hostname;
      Port = host.ssh.port;
      HostKeyAlias = lib.removeSuffix "-wifi" name;
    }
    // lib.optionalAttrs (host.ssh ? user) { User = host.ssh.user; };

  hasHostKey = _: keys: keys ? host;
  toKnownHost = _: { hostNames, publicKey }: "${lib.concatStringsSep "," hostNames} ${publicKey}\n";

in
{
  options = {
    # make home-manager quack like programs.ssh.knownHosts from nixos/nix-darwin
    # probably i should just set it globally but i like it here, ah well
    ssh.knownHosts = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, ... }:
          {
            options = {
              hostNames = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ name ];
              };
              publicKey = lib.mkOption {
                type = lib.types.str;
              };
            };
          }
        )
      );
      default = { };
    };
  };

  config = {
    ssh.knownHosts = lib.mapAttrs (_: keys: { publicKey = keys.host; }) (lib.filterAttrs hasHostKey pd.keys) // {
      github = {
        hostNames = [ "github.com" ];
        publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl";
      };
    };

    home.file.".ssh/known_hosts.d/home".text = lib.concatStrings (
      lib.mapAttrsToList toKnownHost config.ssh.knownHosts
    );

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
          UserKnownHostsFile = [
            "~/.ssh/known_hosts"
            "~/.ssh/known_hosts.d/home"
          ];
          ControlMaster = "no";
          ControlPath = "~/.ssh/master-%r@%n:%p";
          ControlPersist = "no";
        };
      }
      // (lib.mapAttrs hostBlock others);
    };
  };
}
