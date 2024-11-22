{
  config,
  lib,
  net,
  ...
}:
let
  others = removeAttrs net.ssh.hosts [ config.networking.hostName ];
  matchBlock = _: host: host.ssh;
in
{
  home-manager.users.pd.programs.ssh = {
    enable = true;

    extraConfig = ''
      IdentitiesOnly yes
      IdentityFile ~/.ssh/id_ed25519
    '';

    matchBlocks = lib.mapAttrs matchBlock others;
  };
}
