{ ... }:
{
  imports = [
    ../.
    ./homebrew.nix
    ./ula.nix
  ];

  security.sudo.extraConfig = ''
    %admin ALL = (ALL) NOPASSWD: ALL
  '';
}
