# rebind right alt as Mod3, used for xmonad's modMask
xmodmap -e "remove Mod1 = Alt_R"
xmodmap -e "add Mod3 = Alt_R"

xsetroot -cursor_name left_ptr
feh --bg-scale ~/Desktop/background.jpg &

emacs --daemon &

exec xmonad
