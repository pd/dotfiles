xmodmap ~/.Xmodmap

xsetroot -cursor_name left_ptr

# add to x's font path
xset fp+ /usr/share/fonts/local
xset fp+ /usr/share/fonts/TTF
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

# attach the second monitor now if it's available
if xrandr -q | grep "DP-1 connected"; then
  mon-attach
fi

# do it
emacs --daemon=main &
exec xmonad
