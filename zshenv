# zshenv: run for all zsh shells

# OSX's path_helper sucks. The /etc files for it seem to be overwritten
# god-knows-when. Just undo everything it decided to do and fkn start over.
export PATH="$HOME/bin:$HOME/.rbenv/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin"

# Boot rbenv.
# if [ $(command -v rbenv 2>&1) ]; then eval "$(rbenv init - --no-rehash)"; fi

# Boot chruby, actually.
if [ -f /usr/local/share/chruby/chruby.sh ]; then
  source /usr/local/share/chruby/chruby.sh
  source /usr/local/share/chruby/auto.sh

  # See chruby#160; chruby uses preexec, but prompt is printed between precmd
  # and preexec. So, just run chruby_auto twice and my prompt will match correctly.
  precmd_functions+=("chruby_auto")

  [ -f ~/.ruby-version ] && chruby $(cat ~/.ruby-version)
  [ -f ./.ruby-version ] && chruby $(cat ./.ruby-version)
fi
