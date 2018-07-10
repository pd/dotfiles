if [[ -f /usr/local/bin/aws_completer ]]; then
  # on zsh, assumes bashcompinit.
  complete -C /usr/local/bin/aws_completer aws
fi

if [[ -x /usr/local/bin/vault ]]; then
  complete -o nospace -C /usr/local/bin/vault vault
fi
