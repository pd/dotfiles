if [[ -f /usr/local/bin/aws_completer ]]; then
  # on zsh, assumes bashcompinit.
  complete -C /usr/local/bin/aws_completer aws
fi

if [[ -x /usr/local/bin/vault ]]; then
  complete -o nospace -C /usr/local/bin/vault vault
fi

if [[ -x /usr/local/bin/kubectl ]]; then
  source <(kubectl completion zsh)
fi

if [[ -x /usr/local/bin/terraform-docs ]]; then
  source <(terraform-docs completion zsh)
fi

if [[ -f /usr/local/etc/bash_completion.d/az ]]; then
  source /usr/local/etc/bash_completion.d/az
fi

if [[ -f /usr/local/etc/bash_completion.d/asdf.bash ]]; then
  source /usr/local/etc/bash_completion.d/asdf.bash
fi
