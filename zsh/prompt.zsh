## expand %m, %*; expand '$(echo hi)', '${foovar}'
setopt prompt_percent prompt_subst

_prompt_git_current_branch () {
  ref=$(git-symbolic-ref HEAD 2>/dev/null) || return
  echo "(gb: $fg[green]${ref#refs/heads/}$fg[default]) "
}

## okay now
# export PS1='%m %~ $(_prompt_git_current_branch)%# '
export PS1='
-- $fg[yellow][$fg[default] %n @ %m $fg[yellow]]$fg[default] $fg[yellow][$fg[default] $fg[magenta]%~ $fg[yellow]]$fg[default] $(_prompt_git_current_branch)$fg[yellow][$fg[default] %D{%a, %b %d %T} $fg[yellow]]$fg[default] 
-- %(?||(exit: $fg[red]%?$fg[default]%) )%# '
