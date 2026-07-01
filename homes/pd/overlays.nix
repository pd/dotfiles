{ inputs, ... }:
{
  # use emacs-overlay for emacsWithPackagesFromUsePackage; the
  # actual emacs packages we use are still from nixpkgs(-unstable).
  nixpkgs.overlays = [ inputs.emacs-overlay.overlays.default ];
}
