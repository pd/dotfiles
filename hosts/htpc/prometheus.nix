{
  config,
  lib,
  pd,
  ...
}:
let
  inherit (lib) mapAttrsToList;

  maybePort = port: name: if lib.hasInfix ":" name then name else "${name}:${toString port}";
  targets = port: names: map (maybePort port) names;
  staticJob = job_name: port: hosts: {
    inherit job_name;
    scrape_interval = "60s";
    static_configs = [ { targets = targets port hosts; } ];
  };

  ports =
    let
      prometheus = config.services.prometheus;
      exporters = prometheus.exporters;
    in
    {
      caddy = 2020;
      dnsmasq = exporters.dnsmasq.port;
      jellyfin = 8096;
      node-exporter = exporters.node.port;
      ntfy = 9712; # cf donix/ntfy.nix
      prometheus = prometheus.port;
      rtorrent = 9135;
      snmp = 9116;
      wireguard = exporters.wireguard.port;
    };

  macs = lib.concatMapAttrs (
    host: attrs:
    builtins.listToAttrs (
      map (mac: {
        name = mac;
        value = host;
      }) attrs.macs
    )
  ) pd.net.hosts;
in
{
  services.prometheus = {
    enable = true;
    enableReload = true;
    webExternalUrl = "http://prom.home";
    port = 9999;

    scrapeConfigs = [
      (staticJob "caddy" ports.caddy [
        "donix.wg"
        "htpc.home"
      ])
      (staticJob "dnsmasq" ports.dnsmasq [
        "htpc.home"
        "pi.home"
      ])
      (staticJob "jellyfin" ports.jellyfin [ "htpc.home" ])
      (staticJob "ntfy" ports.ntfy [ "donix.wg" ])
      (staticJob "nodes" ports.node-exporter [
        "desk.home"
        "donix.wg"
        "htpc.home"
        "nas.home"
        "orb.home:19100"
        "pi.home"
      ])
      (staticJob "prometheus" ports.prometheus [ "htpc.home" ])
      (
        (staticJob "rtorrent" ports.rtorrent [ "htpc.home" ])
        // {
          metric_relabel_configs = [
            {
              # Drop metrics until rtorrent-exporter has populated the
              # tracker details. Avoids having to explicitly juggle
              # potentially empty tracker labels everywhere.
              action = "drop";
              source_labels = [
                "__name__"
                "tracker"
              ];
              regex = "^rtorrent_downloads_(up|down)load_(total|rate)_bytes;$";
            }
          ];
        }
      )
      (staticJob "snmp-exporter" ports.snmp [ "nas.home" ])
      (
        (staticJob "snmp" ports.snmp [ "nas.home" ])
        // {
          metrics_path = "/snmp";
          params = {
            auth = [ "public_v2" ];
            module = [
              "if_mib"
              "synology"
            ];
            target = [ "tcp://nas.home" ];
          };
        }
      )
      (staticJob "wireguard" ports.wireguard [ "pi.home" ])
      (
        (staticJob "wrt" ports.node-exporter [
          "rpt.home"
          "wrt.home"
        ])
        // {
          metric_relabel_configs =
            [
              {
                action = "lowercase";
                source_labels = [ "mac" ];
                target_label = "mac";
              }

              {
                action = "replace";
                source_labels = [ "instance" ];
                target_label = "router";
                regex = "(.+).home:[0-9]+";
                replacement = "$1";
              }
            ]
            ++ (mapAttrsToList (mac: host: {
              action = "replace";
              source_labels = [ "mac" ];
              target_label = "host";
              regex = mac;
              replacement = host;
            }) macs);
        }
      )
    ];
  };

  services.caddy.virtualHosts."prom.home:80".extraConfig = ''
    reverse_proxy localhost:${toString ports.prometheus}
  '';
}
