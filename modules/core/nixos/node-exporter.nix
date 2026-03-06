{ config, pkgs, ... }:
let
  textfileDir = "/run/prometheus-node-exporter/textfile";
in
{
  services.prometheus.exporters.node = {
    enable = true;
    openFirewall = true;
    listenAddress =
      if config.lan.enable then
        "0.0.0.0"
      else if config.wg.enable then
        config.wg.ipv4
      else
        null;
    enabledCollectors = [
      "systemd"
      "textfile"
    ];
    extraFlags = [
      "--collector.textfile.directory=${textfileDir}"
    ];
  };

  systemd.services.prometheus-nixos = {
    description = "NixOS metrics";
    path = [
      pkgs.jq
      pkgs.nixos-rebuild
    ];
    script = ''
      mkdir -pm 0755 "${textfileDir}"
      DEST="${textfileDir}/generations.prom"
      NEXT="$DEST.next"

      all_gens="$(nixos-rebuild list-generations --json)"
      total="$(echo "$all_gens" | jq -cr length)"
      gen="$(echo "$all_gens" | jq 'map(select(.current))[0]')"
      gen_number="$(echo "$gen" | jq -r .generation)"
      built_at="$(date --date="$(echo "$gen" | jq -r .date)" +%s)"

      echo "# HELP nixos_current_generation Current generation" >> "$NEXT"
      echo "# TYPE nixos_current_generation gauge" >> "$NEXT"
      echo "nixos_current_generation $gen_number" >> "$NEXT"

      echo "# HELP nixos_current_generation_built_seconds Timestamp of when the current generation was built" >> "$NEXT"
      echo "# TYPE nixos_current_generation_built_seconds gauge" >> "$NEXT"
      echo "nixos_current_generation_built_seconds $built_at" >> "$NEXT"

      echo "# HELP nixos_generation_total Total system generations available" >> "$NEXT"
      echo "# TYPE nixos_generation_total gauge" >> "$NEXT"
      echo "nixos_generation_total $total" >> "$NEXT"

      mv "$NEXT" "$DEST"
    '';
    startAt = "*:0/5"; # every 5m
  };
}
