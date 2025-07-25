{ inputs, ... }:
{
  # Add my own packages directly to `pkgs`
  pd = final: _prev: import ../pkgs { pkgs = final; };

  # Expose nixpkgs-unstable as `pkgs.unstable.foo`
  unstable = final: _prev: {
    unstable = import inputs.nixpkgs-unstable {
      inherit (final) system;
      config.allowUnfree = true;
    };
  };
}
