# rebind right alt as Mod3, used for xmonad's modMask
xmodmap -e "remove Mod1 = Alt_R"
xmodmap -e "add Mod3 = Alt_R"

# caps lock -> ctrl
xmodmap -e "remove lock = Caps_Lock"
xmodmap -e "add control = Caps_Lock"

xsetroot -cursor_name left_ptr

# add to x's font path
xset fp+ /usr/share/fonts/local
xset fp rehash

# restore sound settings
alsactl restore

# I constantly switch emacs buffer by accidentally palming the
# touchpad. I rarely click using the touchpad. Disable it.
synclient TapButton1=0

# do it
emacs --daemon=main --eval "(turn-on-save-visited-files-mode)" &
emacs --daemon=irc &
exec xmonad
