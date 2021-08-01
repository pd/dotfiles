## elixir
alias iem='iex -S mix'

## emacs
if [ -n "$INSIDE_EMACS" ]; then
  export PAGER=
  export EDITOR='emacsclient'
fi

em () {
  # If a frame exists, use it. Else create a new one.
  if emacsclient -n -e "(if (> (length (frame-list)) 1) 't)" | grep t >/dev/null 2>&1; then
    emacsclient --alternate-editor='' -n "$@"
  else
    emacsclient --alternate-editor='' -c -n "$@"
  fi
}

# I like toying with spacemacs, but I'm not actually willing to
# switch.
alias spacemacs='/Applications/Emacs.app/Contents/MacOS/Emacs -Q -l ~/spacemacs.d/init.el'

## go
export GOPATH=~/go
alias gt='go test'

## java
if [ -d /usr/local/opt/openjdk@11 ]; then
  export JAVA_HOME="/usr/local/opt/openjdk@11/libexec/openjdk.jdk/Contents/Home"
elif [ -d /usr/local/opt/openjdk@13 ]; then
  export JAVA_HOME="/usr/local/opt/openjdk@13/libexec/openjdk.jdk/Contents/Home"
elif [ -x /usr/libexec/java_home ]; then
  export JAVA_HOME="$(/usr/libexec/java_home --version 1.8+)"
fi

## node
if [ -d /usr/local/lib/node_modules ]; then
  NODE_PATH="/usr/local/lib/node_modules:$NODE_PATH"
fi

if [ -d /usr/local/share/npm/lib/node_modules ]; then
  NODE_PATH="/usr/local/share/npm/lib/node_modules:$NODE_PATH"
fi

## ruby
rerubies () {
  RUBIES=(~/.rubies/*)
}

## rust
alias ct='cargo test'
alias ctnc='cargo test -- --nocapture'
alias cb='cargo build'
alias cr='cargo run'
alias cwtf='env RUST_BACKTRACE=1 cargo test --jobs 1 -- --nocapture'
alias cargocov='cargo +nightly cov clean && cargo +nightly cov test && cargo +nightly cov report --open'

## ops
alias k=kubectl
if which dyff &>/dev/null; then
  export KUBECTL_EXTERNAL_DIFF="dyff between --omit-header --set-exit-code"
fi

# given yaml stream eg `kustomize build . | kgrep ...`,
# return docs of the given kind
kgrep () {
  yq read --doc '*' --tojson - | jq --arg kind "$1" '.[] | select(.kind == $kind)' | yq read -
}
