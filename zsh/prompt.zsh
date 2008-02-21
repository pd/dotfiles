export PS1='%m %~ %# '

# executed before printing a prompt
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
