if [[ -f /usr/local/bin/aws_completer ]]; then
  # on zsh, assumes bashcompinit.
  complete -C /usr/local/bin/aws_completer aws
fi
