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

  output = writeShellScriptBin "wl-screenshot-output" ''
    ${grim}/bin/grim -o "$(${slurp}/bin/slurp -o -f %o)" - | ${wl-clipboard}/bin/wl-copy
  '';
in
symlinkJoin {
  name = "screenshots";
  paths = [
    region
    output
  ];
}
