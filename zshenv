# zshenv: run for all zsh shells
if [ $(command -v rbenv 2>&1) ]; then eval "$(rbenv init - --no-rehash)"; fi
