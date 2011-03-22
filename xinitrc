xmodmap ~/.Xmodmap

xsetroot -cursor_name left_ptr

# add to x's font path
xset fp+ /usr/share/fonts/local
xset fp rehash

# restore sound settings
alsactl restore

# stfu system beep
xset -b

# I constantly switch emacs buffer by accidentally palming the
# touchpad. I rarely click using the touchpad. Disable it.
if [ -x /usr/bin/synclient ]; then
  synclient TapButton1=0
fi

# many java apps boot as grey screens without this
export _JAVA_AWT_VM_NONREPARENTING=1

# do it
emacs --daemon=main --eval "(turn-on-save-visited-files-mode)" &
emacs --daemon=irc &
exec xmonad
