if [ -n "$INSIDE_EMACS" ]; then
  export PAGER=
  export EDITOR='emacsclient'
fi

alias em='emacsclient -n'
