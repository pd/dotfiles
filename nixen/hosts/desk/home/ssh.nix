{ lib, ... }:
let
  net = import ../../../modules/net.nix;
  others = lib.filterAttrs (n: h: n != "desk" && (h.ssh or {}) != false) net.hosts;
in
{
  home-manager.users.pd.programs.ssh = {
    enable = true;

    extraConfig = ''
      IdentitiesOnly yes
      IdentityFile ~/.ssh/id_ed25519
    '';

    matchBlocks = lib.mapAttrs (name: host:
    let ssh = host.ssh or {}; in {
      hostname = ssh.hostname or "${name}.home";
      port = ssh.port or 1222;
    }) others;
  };
}
