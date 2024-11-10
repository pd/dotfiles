{ ... }:
{
  homebrew = {
    enable = true;
    global.autoUpdate = false;

    taps = [
      "d12frosted/emacs-plus"
    ];

    caskArgs = {
      appdir = "~/Applications";
    };

    brews = [
      {
        name = "emacs-plus@30";
        args = [
          "with-native-comp"
          "with-imagemagick"
        ];
      }
      "restish"
    ];

    casks = [
      "alfred"
      "bitwarden"
      "dash"
      "discord"
      "firefox"
      "iterm2"
      "orbstack"
      "neo4j"
      "notion"
      "signal"
      "slack"
      "spectacle"
      "visual-studio-code"
      "vlc"
      "wireshark"
      "zoom"
    ];
  };
}
