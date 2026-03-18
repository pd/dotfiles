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
      "ghostty@tip"
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
      "steam"
      "visual-studio-code"
      "vlc"
      "wireshark-app"
      "zoom"
    ];
  };

  # cask installation doesn't work, let nix-darwin deal with
  # the absurd constraint that it "must" be in /Applications
  programs._1password-gui.enable = true;
}
