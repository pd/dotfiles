{ inputs, ... }:
{
  # Expose nixpkgs-unstable as `pkgs.unstable.foo`
  unstable = final: _prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.stdenv.hostPlatform.system;
      config.allowUnfree = true;

      # and ensure emacs-overlay applies to it as well
      overlays = [ inputs.emacs-overlay.overlays.default ];
    };
  };

  # Add my own packages as `pkgs.pd`
  pd = final: _prev: {
    pd = import ../pkgs {
      pkgs = final;
      pd = import ../lib/pd { inherit (final) lib; };
      inherit (final) unstable;
      inherit (inputs) stevenblack-blocklist hagezi-lgwebos hagezi-pro-plus;
    };
  };
}
