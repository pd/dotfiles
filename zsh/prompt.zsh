## expand %m, %*; expand '$(echo hi)', '${foovar}'
setopt prompt_percent prompt_subst

_prompt_git_current_branch () {
  ref=$(git-symbolic-ref HEAD 2>/dev/null) || return
  echo "(gb: ${ref#refs/heads/}) "
}

## okay now
export PS1='%m %~ $(_prompt_git_current_branch)%# '

## executed before printing a prompt
precmd () {
  str=`print -P '%m: %~'`
  iterm_set_tab_label $str
  iterm_set_window_title $str
}

## executed just after reading a command, before running it
preexec () {
  cmd=$(history $HISTCMD | cut -b7-)
  str=`print -P "%m: $cmd"`
  iterm_set_tab_label $str
  iterm_set_window_title $str
}
