if [ -d /usr/local/lib/node_modules ]; then
  NODE_PATH="/usr/local/lib/node_modules:$NODE_PATH"
fi

if [ -d /usr/local/share/npm/lib/node_modules ]; then
  NODE_PATH="/usr/local/share/npm/lib/node_modules:$NODE_PATH"
fi

if [ -z "$EMACS" ]; then
  alias node='rlwrap node'
fi

alias tracegl='node ~/vendor/tracegl/tracegl.js -nolib -no:repl.js'
alias nt="npm test"

function bower-search () {
  bower search --color "$@" | sed 's,git://github.com,https://github.com,'
}
