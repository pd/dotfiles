if [ -r ~/.Xmodmap ]; then
  xmodmap ~/.Xmodmap
fi

emacs --daemon
exec i3
