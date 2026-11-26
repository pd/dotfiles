{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  homeDir = config.home.homeDirectory;
  lnDot = path: { source = config.lib.file.mkOutOfStoreSymlink "${homeDir}/dotfiles/${path}"; };

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
        systemctl --user start emacs.service
        emacsclient --no-wait --reuse-frame "$@"
      ''
  );
in
{
  home.file = {
    ".emacs.d" = lnDot "emacs.d";
  };

  programs.emacs = {
    enable = true;
    package = pkgs.unstable.emacsWithPackagesFromUsePackage {
      package = if isDarwin then pkgs.unstable.emacs31 else pkgs.unstable.emacs31-pgtk;
      config = ../../emacs.d/init.el;
      alwaysEnsure = true;
      extraEmacsPackages =
        epkgs: with epkgs; [
          treesit-grammars.with-all-grammars
          ghostel
          evil-ghostel
        ];

      # should land in unstable soon but i want it now
      override = final: prev: {
        ghostel = prev.ghostel.overrideAttrs (
          finalAttrs: _: {
            version = "0.67.0";
            src = pkgs.fetchFromGitHub {
              owner = "dakra";
              repo = "ghostel";
              tag = "v${finalAttrs.version}";
              hash = "sha256-GjS6SYoCwmRrP2P7SOFTtStHDPtuy6Uq52Sug8eYfYE=";
            };
            zig = pkgs.unstable.zig_0_17;
            zigDeps = finalAttrs.zig.fetchDeps {
              inherit (finalAttrs) src pname version;
              fetchAll = true;
              hash = "sha256-3pDE8/KCzHR08VqAHJR6rdMOGARCyH0tc5UTOeCvs14=";
            };
          }
        );
        consult-ghostel = prev.consult-ghostel.overrideAttrs {
          inherit (final.ghostel) src version;
        };
      };
    };
  };

  home.packages =
    with pkgs;
    [
      emacs-all-the-icons-fonts
      go-grip # used by grip-mode
    ]
    ++ [ em ];

  services.emacs = {
    enable = true;
    startWithUserSession = "graphical";
  };

  # Launch daemon from .app bundle so macOS gives it a proper bundle ID and icon.
  launchd.agents.emacs.config.ProgramArguments = lib.mkIf isDarwin (
    lib.mkForce [
      "${homeDir}/Applications/Home Manager Apps/Emacs.app/Contents/MacOS/Emacs"
      "--fg-daemon"
    ]
  );
}
