## expand %m, %*; expand '$(echo hi)', '${foovar}'
setopt prompt_percent prompt_subst

_prompt_git_current_branch () {
  ref=$(git-symbolic-ref HEAD 2>/dev/null) || return
  echo "(gb: $fg[green]${ref#refs/heads/}$reset_color) "
}

## iterm
##   \e]1 = tab label
##   \e]2 = window title
iterm_set_tab_label () {
  echo -ne "\e]1;$*\a"
}
iterm_set_window_title () {
  echo -ne "\e]2;$*\a"
}

# executed just before printing a prompt
precmd () {
  str=`print -P '%m: %~'`
  iterm_set_tab_label $str
  iterm_set_window_title $str
}

# executed just after reading a command, before running it
preexec () {
  cmd=$(history $HISTCMD | cut -b7-)
  str=`print -P "%m: $cmd"`
  iterm_set_tab_label $str
  iterm_set_window_title $str
}

## okay now
export PS1='
-- %{$fg_bold[yellow]%}[%{$reset_color%} %n @ %m %{$fg[green]%}%~ %{$fg_bold[yellow]%}]%{$reset_color%} $(_prompt_git_current_branch)%{$fg_bold[yellow]%}[%{$reset_color%} %D{%a, %b %d %T} %{$fg_bold[yellow]%}]%{$reset_color%}%(1j. !! has a job.)
-- %(?||(\$!: %{$fg_bold[grey]%}%?%{$reset_color%}%) )%# '
