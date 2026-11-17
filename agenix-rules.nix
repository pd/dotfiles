let
  keys = import ./lib/pd/keys.nix;
  me = keys.armspan.pd.ssh ++ keys.desk.pd.ssh ++ builtins.attrValues keys.yubis;

  rules =
    kind: dir: keys: secrets:
    builtins.listToAttrs (
      map (name: {
        name = "${kind}/${dir}/secrets/${name}.age";
        value.publicKeys = keys;
      }) secrets
    );

  forHost = host: secrets: rules "hosts" host (me ++ [ keys.${host}.host ]) secrets;
  forHome = home: secrets: rules "homes" home me secrets;
in
{
  ".envrc.age".publicKeys = me;
  "routers/wrt/secrets.json.age".publicKeys = me;
  "routers/rpt/secrets.json.age".publicKeys = me;
}
// (forHost "armspan" [
  "wireguard-private-key"
  "wireguard-preshared-key"
])
// (forHost "desk" [
  "desktop-password"
  "wifi"
])
// (forHost "donix" [
  "gatus.env"
  "ntfy-auth.env"
  "wireguard-private-key"
  "wireguard-preshared-key"
])
// (forHost "htpc" [
  "alertmanager-ntfy"
  "filebot-license"
  "filebotd.env"
  "gatus-heartbeat.env"
  "harmonia-key"
  "immich.env"
  "koito.env"
  "multiscrobbler.env"
  "npd.env"
  "psql-init.sql"
  "qbittorrent-exporter.env"
  "qbt-categories"
  "restic-password"
  "restic.env"
  "wifi"
  "wireguard-preshared-key"
  "wireguard-private-key"
])
// (forHost "pi" [
  "gatus-heartbeat.env"
  "wireguard-private-key"
  "wireguard-preshared-key-armspan"
  "wireguard-preshared-key-donix"
  "wireguard-preshared-key-htpc"
  "wireguard-preshared-key-pdroid"
])
// (forHome "pd@desk" [
  "PTPIMG_API_KEY"
  "OPS_ANNOUNCE_URL"
])
