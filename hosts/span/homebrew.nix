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
    ];

    casks = [
      "alfred"
      "bitwarden"
      "dash"
      "discord"
      "firefox"
      "iterm2"
      "orbstack"
      "notion"
      "notion-calendar"
      "signal"
      "slack"
      "spectacle"
      "visual-studio-code"
      "vlc"
      "wireshark-app"
      "zoom"
    ];
  };
}
