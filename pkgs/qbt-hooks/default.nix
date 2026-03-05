{
  symlinkJoin,
  writeShellApplication,
  curl,
  gnused,
  gnugrep,
}:
let
  qbt-on-add = writeShellApplication {
    name = "qbt-on-add";
    runtimeInputs = [ curl gnused gnugrep ];
    text = builtins.readFile ./qbt-on-add.sh;
  };

  qbt-on-complete = writeShellApplication {
    name = "qbt-on-complete";
    runtimeInputs = [ curl ];
    text = builtins.readFile ./qbt-on-complete.sh;
  };
in
symlinkJoin {
  name = "qbt-hooks";
  paths = [ qbt-on-add qbt-on-complete ];
}
