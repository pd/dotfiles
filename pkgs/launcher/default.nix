{
  symlinkJoin,
  writeShellApplication,

  cliphist,
  fuzzel,
  jq,
  xdg-utils,
}:
let
  search-menu = writeShellApplication {
    name = "search-menu";
    runtimeInputs = [
      jq
      xdg-utils
    ];
    text = builtins.readFile ./search-menu.sh;
  };
in
symlinkJoin {
  name = "launcher";
  paths = [
    fuzzel
    cliphist
    search-menu
  ];
}
