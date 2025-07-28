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

    extraConfig = ''
      IdentitiesOnly yes
      IdentityFile ~/.ssh/id_ed25519
    '';

    matchBlocks = lib.mapAttrs matchBlock others;
  };
}
