## expand %m, %*; expand '$(echo hi)', '${foovar}'
setopt prompt_percent prompt_subst

## emacs shell == "dumb"
# disable zsh line editing and let emacs do it
[[ $TERM = "dumb" ]] && unsetopt zle
if [[ -n $SSH_CONNECTION ]]; then
  export PS1='%~ @ %m » '
else
  export PS1='%~ » '
fi
