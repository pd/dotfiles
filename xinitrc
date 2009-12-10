# rebind right alt as Mod3, used for xmonad's modMask
xmodmap -e "remove Mod1 = Alt_R"
xmodmap -e "add Mod3 = Alt_R"

# caps lock -> ctrl
xmodmap -e "remove lock = Caps_Lock"
xmodmap -e "add control = Caps_Lock"

xsetroot -cursor_name left_ptr

emacs --daemon &

exec xmonad
