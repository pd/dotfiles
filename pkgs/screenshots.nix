{
  writeShellScriptBin,
  symlinkJoin,

  grim,
  slurp,
  wl-clipboard,
}:
let
  region = writeShellScriptBin "wl-screenshot-region" ''
    ${grim}/bin/grim -g "$(${slurp}/bin/slurp)" - | ${wl-clipboard}/bin/wl-copy
  '';

  display = writeShellScriptBin "wl-screenshot-display" ''
    ${grim}/bin/grim - | ${wl-clipboard}/bin/wl-copy
  '';
in
symlinkJoin {
  name = "screenshots";
  paths = [
    region
    display
  ];
}
