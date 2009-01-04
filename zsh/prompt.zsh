## expand %m, %*; expand '$(echo hi)', '${foovar}'
setopt prompt_percent prompt_subst

_prompt_git_info () {
  local g="$(git rev-parse --git-dir 2>/dev/null)"
  if [ -n "$g" ]; then
    local r
    local b

    if [ -d "$g/../.dotest" ]
    then
      if test -f "$g/../.dotest/rebasing"
      then
        r=" | REBASE"
      elif test -f "$g/../.dotest/applying"
      then
        r=" | AM"
      else
        r=" | AM/REBASE"
      fi
      b="$(git symbolic-ref HEAD 2>/dev/null)"
    elif [ -f "$g/.dotest-merge/interactive" ]
    then
      r=" | REBASE-i"
      b="$(cat "$g/.dotest-merge/head-name")"
    elif [ -d "$g/.dotest-merge" ]
    then
      r=" | REBASE-m"
      b="$(cat "$g/.dotest-merge/head-name")"
    elif [ -f "$g/MERGE_HEAD" ]
    then
      r=" | MERGING"
      b="$(git symbolic-ref HEAD 2>/dev/null)"
    else
      if [ -f "$g/BISECT_LOG" ]
      then
        r=" | BISECTING"
      fi
      if ! b="$(git symbolic-ref HEAD 2>/dev/null)"
      then
        if ! b="tag: $(git describe --exact-match HEAD 2>/dev/null)"
        then
          b="$(cut -c1-7 "$g/HEAD")..."
        fi
      fi
    fi

    if [ -n "$1" ]; then
      printf "$1" "${b##refs/heads/}$r"
    else
      printf "($fg[green]%s$reset_color) " "${b##refs/heads/}$r"
    fi
  fi
}

## works in at least Terminal.app and iTerm
##   \e]1 = tab label
##   \e]2 = window title
terminal_set_tab_label () {
  echo -ne "\e]1;$*\a"
}
terminal_set_window_title () {
  echo -ne "\e]2;$*\a"
}

## only use the iterm commands when in Terminal.app
## what a misnomer eh

if [[ $TERM = "xterm-color" ]]; then
  # executed just before printing a prompt
  precmd () {
    str=`print -P '%m: %~'`
    terminal_set_tab_label $str
    terminal_set_window_title $str
  }

  # executed just after reading a command, before running it
  preexec () {
    cmd=$(history $HISTCMD | cut -b7-)
    str=`print -P "%m: $cmd"`
    terminal_set_tab_label $str
    terminal_set_window_title $str
  }
fi

if [[ $TERM = "eterm-color" ]]; then
  eterm-set-cwd () {
    $@
    echo -e "\033AnSiTc" $(pwd)
  }

  eterm-reset () {
    echo -e "\033AnSiTu" $(whoami)
    echo -e "\033AnSiTc" $(pwd)
    echo -e "\033AnSiTh" $(hostname -s)
  }

  for d in cd pushd popd; do
    alias $d="eterm-set-cwd $d"
  done

  eterm-reset
fi

## okay now
export PS1='
-- %{$fg_bold[blue]%}[%{$reset_color%} %n @ %m %{$fg[green]%}%~ %{$fg_bold[blue]%}]%{$reset_color%} $(_prompt_git_info)%{$fg_bold[blue]%}[%{$reset_color%} %D{%a, %b %d %T} %{$fg_bold[blue]%}]%{$reset_color%}
-- %(?||(\$!: %{$fg[red]%}%?%{$reset_color%}%) )%# '
