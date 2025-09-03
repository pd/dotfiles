{
  symlinkJoin,
  writeShellApplication,

  cliphist,
  fuzzel,
  gawk,
  imagemagick,
  jq,
  wl-clipboard,
  xdg-utils,
}:
let
  cliphist-fuzzel-img = writeShellApplication {
    name = "cliphist-fuzzel-img";
    runtimeInputs = [
      cliphist
      fuzzel
      gawk
      imagemagick
      wl-clipboard
    ];

    bashOptions = [
      "nounset"
      "pipefail"
    ];

    text = builtins.readFile ./cliphist-fuzzel-img.sh;
  };

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
    cliphist-fuzzel-img
    search-menu
  ];
}
