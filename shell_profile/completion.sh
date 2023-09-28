if [[ -f /usr/local/bin/aws_completer ]]; then
  # on zsh, assumes bashcompinit.
  complete -C /usr/local/bin/aws_completer aws
fi

if [[ -x /usr/local/bin/vault ]]; then
  complete -o nospace -C /usr/local/bin/vault vault
fi

if [[ -f /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc ]]; then
  source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc
fi

if [[ -f /usr/local/etc/bash_completion.d/az ]]; then
  source /usr/local/etc/bash_completion.d/az
fi

if [[ -f /usr/local/etc/bash_completion.d/asdf.bash ]]; then
  source /usr/local/etc/bash_completion.d/asdf.bash
fi

if which fly &>/dev/null; then
  source <(fly completion --shell=zsh)
fi

if which kubectl &>/dev/null; then
  source <(kubectl completion zsh)
fi

if [[ -f /usr/local/opt/fzf/shell/completion.zsh ]]; then
  source /usr/local/opt/fzf/shell/completion.zsh
  source /usr/local/opt/fzf/shell/key-bindings.zsh
fi
