export LC_CTYPE=en_US.UTF-8

## emacs
if [[ -n "$INSIDE_EMACS" ]]; then
  export EDITOR='emacsclient'
  if [[ "$INSIDE_EMACS" != "vterm" ]]; then
    export PAGER=
  fi
elif [[ -z "$EDITOR" ]]; then
  export EDITOR=vim
fi

em () {
  # If a frame exists, use it. Else create a new one.
  if emacsclient -n -e "(if (> (length (frame-list)) 1) 't)" | grep t >/dev/null 2>&1; then
    emacsclient --alternate-editor='' -n "$@"
  else
    emacsclient --alternate-editor='' -c -n "$@"
  fi
}

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

## rust
export CARGO_NET_GIT_FETCH_WITH_CLI=true
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
export USE_GKE_GCLOUD_AUTH_PLUGIN=True

if [[ -f /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc ]]; then
  source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc
  source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc
fi
