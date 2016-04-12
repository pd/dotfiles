if command -v kitchen >/dev/null 2>&1; then
  alias kls='kitchen list'
  alias kc='kitchen converge'
  alias kv='kitchen verify'
  alias kl='kitchen login'
  alias kd='kitchen destroy'

  # kitchen converge foo && kitchen verify foo
  kcv () {
    kitchen converge "${1}" && kitchen verify "${1}"
  }
fi
