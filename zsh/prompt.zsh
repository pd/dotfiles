## expand %m, %*; expand '$(echo hi)', '${foovar}'
setopt prompt_percent prompt_subst

## emacs shell == "dumb"
# disable zsh line editing and let emacs do it
[[ $TERM = "dumb" ]] && unsetopt zle
export PS1='%~%(?|| (\$!: %{$fg[red]%}%?%{$reset_color%}%)) %# '
