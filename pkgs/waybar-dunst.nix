{
  dbus,
  dunst,
  lib,
  pkgs,
}:
let
  dunstctl = lib.getExe' dunst "dunstctl";
  monitor = lib.getExe' dbus "dbus-monitor";
in
pkgs.writeShellScriptBin "waybar-dunst" ''
  render() {
    if [[ "$(${dunstctl} is-paused)" == "true" ]]; then
      printf '{"text":"%s"}\n' "󰂛"
    else
      printf '{"text":"%s"}\n' "󰂚"
    fi
  }

  render

  # Lifted from:
  # https://github.com/Keyruu/shinyflakes/blob/15f3a8a5c5d24c228c77b532b7477892336145be/home/linux/scripts/notif.nix#L27
  exec ${monitor} --profile "path='/org/freedesktop/Notifications',interface='org.freedesktop.DBus.Properties',member='PropertiesChanged'" | \
  while read -r _; do
    render
  done
''
