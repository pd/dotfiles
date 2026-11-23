{
  lib,
  writeShellApplication,
  wakeonlan,

  macs,
}:
writeShellApplication {
  name = "wake-desk";
  runtimeInputs = [ wakeonlan ];
  text = ''
    wakeonlan ${lib.escapeShellArgs macs}
  '';
}
