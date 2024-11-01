{ lib, net, ... }:
let
  others = removeAttrs net.ssh.hosts [ "desk" ];
  matchBlock =
    name: host:
    {
      port = 1222;
      hostname = "${name}.home";
    }
    // (host.ssh or { });
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
