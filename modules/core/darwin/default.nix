{ ... }:
{
  imports = [
    ../.
    ./homebrew.nix
  ];

  security.sudo.extraConfig = ''
    %admin ALL = (ALL) NOPASSWD: ALL
  '';
}
