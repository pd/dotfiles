let
  keys = import ./lib/pd/keys.nix;
  me = keys.armspan.pd.ssh ++ keys.desk.pd.ssh ++ builtins.attrValues keys.yubis;
  forHost =
    host: names:
    builtins.listToAttrs (
      map (n: {
        name = "hosts/${host}/secrets/${n}.age";
        value.publicKeys = me ++ [ keys.${host}.host ];
      }) names
    );
in
forHost "armspan" [
  "wireguard-private-key"
  "wireguard-preshared-key"
]
