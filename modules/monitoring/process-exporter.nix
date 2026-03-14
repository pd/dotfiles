{ config, lib, ... }:
let
  cfg = config.monitoring.processes;
in
{
  options.monitoring.processes = lib.mkOption {
    type = lib.types.attrsOf (lib.types.attrsOf (lib.types.listOf lib.types.str));
    default = { };
    description = "Processes to monitor with prometheus process-exporter.";
  };

  config = lib.mkIf (cfg != { }) {
    services.prometheus.exporters.process = {
      enable = true;
      openFirewall = true;
      settings.process_names = lib.mapAttrsToList (name: attrs: attrs // { inherit name; }) cfg;
    };
  };
}
