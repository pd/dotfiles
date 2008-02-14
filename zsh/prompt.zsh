export PS1='kyleh %~ %# '

# executed before printing a prompt
function precmd {
  host="kyleh"
  _pwd=`echo $PWD | sed "s,$HOME,~,"`
  str="$host: $_pwd"
  iterm_set_tab_label $str
  iterm_set_window_title $str
}

# executed just after reading a command, before running it
function preexec {
  host="kyleh"
  cmd=$(history $HISTCMD | cut -b7-)
  str="$host: $cmd"
  iterm_set_tab_label $str
  iterm_set_window_title $str
}
