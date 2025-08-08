{ inputs, ... }:
{
  # Add my own packages as `pkgs.pd`
  pd = final: _prev: {
    pd = import ../pkgs {
      pkgs = final;
      unstable = final.unstable;
    };
  };

  # Expose nixpkgs-terraform as `pkgs.terraform-versions`
  nixpkgs-terraform = final: _prev: {
    terraform-versions = inputs.nixpkgs-terraform.packages.${final.system};
  };

  # Expose nixpkgs-unstable as `pkgs.unstable.foo`
  unstable = final: _prev: {
    unstable = import inputs.nixpkgs-unstable {
      inherit (final) system;
      config.allowUnfree = true;
    };
  };
}
