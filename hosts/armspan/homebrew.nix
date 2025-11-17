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
      "amazon-workspaces"
      "bitwarden"
      "dash"
      "discord"
      "firefox"
      "google-chrome"
      "iterm2"
      "loom"
      "notion"
      "notion-calendar"
      "orbstack"
      "pinta"
      "rectangle"
      "signal"
      "slack"
      "visual-studio-code"
      "vlc"
      "wireshark-app"
      "zoom"
    ];
  };
}
