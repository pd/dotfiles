if [ -d /usr/local/lib/node_modules ]; then
  NODE_PATH="/usr/local/lib/node_modules:$NODE_PATH"
fi

if [ -d /usr/local/share/npm/lib/node_modules ]; then
  NODE_PATH="/usr/local/share/npm/lib/node_modules:$NODE_PATH"
fi
