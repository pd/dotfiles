{ inputs, ... }:
{
  # Add my own packages directly to `pkgs`
  pd = final: _prev: {
    pd = import ../pkgs {
      pkgs = final;
      unstable = final.unstable;
    };
  };

  # Expose nixpkgs-unstable as `pkgs.unstable.foo`
  unstable = final: _prev: {
    unstable = import inputs.nixpkgs-unstable {
      inherit (final) system;
      config.allowUnfree = true;
    };
  };
}
