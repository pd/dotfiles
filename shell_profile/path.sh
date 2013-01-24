add_to_path_if_exists () {
  [ -d $1 ] && PATH="$1:$PATH"
}

add_to_path_if_exists /usr/local/sbin
add_to_path_if_exists /usr/local/bin
add_to_path_if_exists /usr/local/texlive/2011/bin/x86_64-darwin
add_to_path_if_exists /usr/local/heroku/bin
add_to_path_if_exists ~/.carton/bin
add_to_path_if_exists ~/dotfiles/vendor/ecukes
add_to_path_if_exists ~/local/bin
add_to_path_if_exists ~/bin
