# zshenv: run for all zsh shells

# OSX's path_helper sucks. The /etc files for it seem to be overwritten
# god-knows-when. Just undo everything it decided to do and fkn start over.
export PATH="$HOME/bin:$HOME/.rbenv/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin"

# Boot rbenv.
if [ $(command -v rbenv 2>&1) ]; then eval "$(rbenv init - --no-rehash)"; fi
