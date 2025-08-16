{ ... }:
{
  homebrew = {
    enable = true;
    global.autoUpdate = false;

    caskArgs = {
      appdir = "~/Applications";
    };

    casks = [
      "alfred"
      "bitwarden"
      "dash"
      "discord"
      "firefox"
      "google-chrome"
      "iterm2"
      "notion"
      "notion-calendar"
      "orbstack"
      "pinta"
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
