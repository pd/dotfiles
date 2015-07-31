if [ -n "$INSIDE_EMACS" ]; then
  export PAGER=
  export EDITOR='emacsclient'
fi

alias em='emacsclient -n'


# I like toying with spacemacs, but I'm not actually willing to
# switch.
alias spacemacs='/Applications/Emacs.app/Contents/MacOS/Emacs -Q -l ~/spacemacs.d/init.el'
