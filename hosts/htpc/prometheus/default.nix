{ ... }:
{
  imports = [
    ./prometheus.nix
    ./alertmanager.nix
    ./process-exporter.nix
  ];
}
