## expand %m, %*; expand '$(echo hi)', '${foovar}'
setopt prompt_percent prompt_subst

_prompt_git_current_branch () {
  ref=$(git-symbolic-ref HEAD 2>/dev/null) || return
  echo "(gb: ${ref#refs/heads/}) "
}

## okay now
export PS1='%m %~ $(_prompt_git_current_branch)%# '
