{ inputs, ... }:
{
  # Expose nixpkgs-unstable as `pkgs.unstable.foo`
  unstable = final: _prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.stdenv.hostPlatform.system;
      config.allowUnfree = true;
    };
  };

  # Add my own packages as `pkgs.pd`
  pd = final: _prev: {
    pd = import ../pkgs {
      pkgs = final;
      unstable = final.unstable;
      stevenblack-blocklist = inputs.stevenblack-blocklist;
    };
  };
}
