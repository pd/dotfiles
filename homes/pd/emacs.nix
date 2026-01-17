{
  config,
  pkgs,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  homeDir = config.home.homeDirectory;
  lnDot = f: { source = config.lib.file.mkOutOfStoreSymlink "${homeDir}/dotfiles/${f}"; };
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

  home.packages = with pkgs; [ emacs-all-the-icons-fonts ];

  services.emacs = {
    enable = true;
    startWithUserSession = "graphical";
  };
}
