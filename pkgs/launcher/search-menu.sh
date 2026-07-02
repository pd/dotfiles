searches() {
  echo 'nixos '
  echo 'nixpkgs '
  echo 'home-manager '
  echo 'nixgh '
  echo 'nixdarwin '
}


q="$(searches | fuzzel --dmenu --fuzzy-min-length=2)"
term="$(echo "$q" | cut -d' ' -f2- | jq -Rr @uri)"

version=26.05
case "$q" in
  nixos*) url="https://search.nixos.org/options?channel=${version}&query=${term}";;
  nixpkgs*) url="https://search.nixos.org/packages?channel=${version}&query=${term}";;
  home-manager*) url="https://home-manager-options.extranix.com/?release=release-${version}&query=${term}";;
  nixgh*) url="https://github.com/search?type=code&q=language%3Anix%20${term}";;
  nixdarwin*) url="https://searchix.alanpearce.eu/options/darwin/search?query=${term}";;
  *) exit 1;;
esac

xdg-open "$url"
