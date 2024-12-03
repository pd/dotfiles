searches() {
  echo 'nixos '
  echo 'nixpkgs '
  echo 'home-manager '
  echo 'nixgh '
  echo 'nixdarwin '
}

q="$(searches | fuzzel --dmenu --fuzzy-min-length=2)"
term="$(echo "$q" | cut -d' ' -f2- | jq -Rr @uri)"

case "$q" in
  nixos*) url="https://search.nixos.org/options?channel=24.11&query=${term}";;
  nixpkgs*) url="https://search.nixos.org/packages?channel=24.11&query=${term}";;
  home-manager*) url="https://home-manager-options.extranix.com/?release=release-24.11&query=${term}";;
  nixgh*) url="https://github.com/search?type=code&q=language%3Anix%20${term}";;
  nixdarwin*) url="https://searchix.alanpearce.eu/options/darwin/search?query=${term}";;
  *) exit 1;;
esac

xdg-open "$url"
