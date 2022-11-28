prepend_path () {
  if [[ -d "$1" && ":$PATH:" != *":$1:"* ]]; then
    PATH="$1:$PATH"
  fi
}

# OSX's path_helper is full on evil. It's run from /etc/zshrc, so every
# interactive shell, and it aggressively pushes `/bin:/usr/bin` etc to the
# front of $PATH which is THE EXACT WORST OPTION WHY WOULD YOU DO THAT FFS.
# Just reset $PATH completely and build from scratch. Twice per shell,
# because we _also_ have to do this from zshenv ...
export PATH="/usr/bin:/bin:/usr/sbin:/sbin"

prepend_path /usr/local/sbin
prepend_path /usr/local/bin
prepend_path /usr/local/texlive/2011/bin/x86_64-darwin
prepend_path /usr/local/opt/postgresql-9.3/bin
prepend_path /usr/local/opt/mysql-client@5.7/bin
prepend_path /usr/local/opt/sqlite/bin
prepend_path /usr/local/heroku/bin
prepend_path /usr/local/share/npm/bin
prepend_path /usr/local/opt/openjdk@11/bin
prepend_path ~/.cask/bin
prepend_path ~/local/bin
prepend_path ~/anaconda/bin
prepend_path ~/.cargo/bin
prepend_path ~/.multirust/bin
prepend_path ~/.multirust/toolchains/nightly/cargo/bin
prepend_path ~/.multirust/toolchains/stable/cargo/bin
prepend_path /usr/local/opt/go/libexec/bin
prepend_path ${GOPATH:-"$HOME/go"}/bin
prepend_path ~/bin
prepend_path "$HOME/Applications/Emacs.app/Contents/MacOS/bin"
prepend_path "$HOME/sauce/vendor/kotlin-language-server/server/build/install/server/bin"
prepend_path "$HOME/.krew/bin"
prepend_path "$HOME/.rd/bin" # rancher desktop

if [[ -d "/usr/local/opt/asdf" ]]; then
  source /usr/local/opt/asdf/asdf.sh
fi
