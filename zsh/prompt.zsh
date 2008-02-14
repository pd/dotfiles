_prompt_host=`hostname -s`
export PS1="$_prompt_host %~ %# "

# executed before printing a prompt
precmd () {
  _pwd=`echo $PWD | sed "s,$HOME,~,"`
  str="$_prompt_host: $_pwd"
  iterm_set_tab_label $str
  iterm_set_window_title $str
}

# executed just after reading a command, before running it
preexec () {
  cmd=$(history $HISTCMD | cut -b7-)
  str="$_prompt_host: $cmd"
  iterm_set_tab_label $str
  iterm_set_window_title $str
}
