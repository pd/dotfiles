{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  homeDir = config.home.homeDirectory;
  lnDot = f: { source = config.lib.file.mkOutOfStoreSymlink "${homeDir}/dotfiles/${f}"; };

  em = pkgs.writeShellScriptBin "em" (
    if isDarwin then
      ''
        # force the service to launch emacs, so it flows through the app bundle
        # and thus gets the correct icon in the dock
        launchctl start org.nix-community.home.emacs
        while ! emacsclient -e nil >/dev/null 2>&1; do
          sleep 0.1
        done

        # Inexplicably, `--reuse-frame` does the opposite on MacOS (as of Emacs 30).
        # It's not just me, apparently:
        # https://emacs.stackexchange.com/questions/79292/why-is-emacsclient-not-reusing-the-existing-frame
        if emacsclient -n -e "(if (> (length (frame-list)) 1) 't)" | grep t >/dev/null 2>&1; then
          emacsclient --no-wait "$@"
        else
          emacsclient --create-frame --no-wait "$@"
        fi
      ''
    else
      ''
        emacsclient --alternate-editor="" --no-wait --reuse-frame "$@"
      ''
  );
in
{
  home.file = {
    ".emacs.d" = lnDot "emacs.d";
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      # on darwin, emacs-30 from nix-darwin-emacs
      # on linux, emacs30-pgtk from nixpkgs-unstable (*not* emacs-overlay)
      # cf. overlays in flake.nix's mkHome
      package = if isDarwin then pkgs.emacs-30 else pkgs.unstable.emacs30-pgtk;
      config = ../../emacs.d/init.el;
      alwaysEnsure = true;
      defaultInitFile = false;
      extraEmacsPackages =
        epkgs: with epkgs; [
          treesit-grammars.with-all-grammars
          vterm
        ];
    };
  };

  home.packages = with pkgs; [ emacs-all-the-icons-fonts ] ++ [ em ];

  services.emacs = {
    enable = true;
    startWithUserSession = "graphical";
  };

  # Launch daemon from .app bundle so macOS gives it a proper bundle ID and icon.
  launchd.agents.emacs.config.ProgramArguments = lib.mkIf isDarwin (
    lib.mkForce [
      "${config.programs.emacs.finalPackage}/Applications/Emacs.app/Contents/MacOS/Emacs"
      "--fg-daemon"
    ]
  );
}
