if command -v kitchen >/dev/null 2>&1; then
  alias kc='kitchen converge'
  alias kv='kitchen verify'

  # kitchen converge foo && kitchen verify foo
  kcv () {
    kitchen converge "${1}" && kitchen verify "${1}"
  }
fi
