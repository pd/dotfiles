add_to_path_if_exists () {
  [ -d $1 ] && PATH="$1:$PATH"
}

add_to_path_if_exists /usr/local/sbin
add_to_path_if_exists /usr/local/bin
add_to_path_if_exists /usr/local/texlive/2011/bin/x86_64-darwin
add_to_path_if_exists /usr/local/opt/postgresql-9.3/bin
add_to_path_if_exists /usr/local/opt/sqlite/bin
add_to_path_if_exists /usr/local/heroku/bin
add_to_path_if_exists /usr/local/share/npm/bin
add_to_path_if_exists ~/.cask/bin
add_to_path_if_exists ~/local/bin
add_to_path_if_exists /opt/chefdk/bin
add_to_path_if_exists ~/anaconda/bin
add_to_path_if_exists ~/.multirust/bin
add_to_path_if_exists ~/.multirust/toolchains/nightly/cargo/bin
add_to_path_if_exists ~/.multirust/toolchains/stable/cargo/bin
add_to_path_if_exists /usr/local/opt/go/libexec/bin
add_to_path_if_exists $GOPATH/bin
add_to_path_if_exists ~/bin
