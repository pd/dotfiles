prepend_path () {
  if [[ -d "$1" && ":$PATH:" != *":$1:"* ]]; then
    PATH="$1:$PATH"
  fi
}

prepend_path /usr/local/sbin
prepend_path /usr/local/bin
prepend_path /usr/local/texlive/2011/bin/x86_64-darwin
prepend_path /usr/local/opt/postgresql-9.3/bin
prepend_path /usr/local/opt/sqlite/bin
prepend_path /usr/local/heroku/bin
prepend_path /usr/local/share/npm/bin
prepend_path ~/.cask/bin
prepend_path ~/local/bin
prepend_path /opt/chefdk/bin
prepend_path ~/anaconda/bin
prepend_path ~/.cargo/bin
prepend_path ~/.multirust/bin
prepend_path ~/.multirust/toolchains/nightly/cargo/bin
prepend_path ~/.multirust/toolchains/stable/cargo/bin
prepend_path /usr/local/opt/go/libexec/bin
prepend_path $GOPATH/bin
prepend_path ~/bin
