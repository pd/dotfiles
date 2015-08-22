if [ -n "$INSIDE_EMACS" ]; then
  export PAGER=
  export EDITOR='emacsclient'
fi

em () {
  # If a frame exists, use it. Else create a new one.
  if emacsclient -n -e "(if (> (length (frame-list)) 1) 't)" | grep t >/dev/null 2>&1; then
    emacsclient --alternate-editor='' -n "$@"
  else
    emacsclient --alternate-editor='' -c -n "$@"
  fi
}

# I like toying with spacemacs, but I'm not actually willing to
# switch.
alias spacemacs='/Applications/Emacs.app/Contents/MacOS/Emacs -Q -l ~/spacemacs.d/init.el'
