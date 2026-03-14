{ ... }:
{
  monitoring.processes = {
    caddy.comm = [ "caddy" ];
    dnsmasq.comm = [ "dnsmasq" ];
    grafana.comm = [ "grafana" ];
    immich.comm = [ "immich" ];
    jellyfin.comm = [ "jellyfin" ];
    postgres.comm = [ "postgres" ];
    prometheus.comm = [ "prometheus" ];
    qbittorrent.cmdline = [ "^/[^ ]+/bin/qbittorrent-nox" ];
  };
}
