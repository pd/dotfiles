NVM_DIR=~/.nvm
if [[ -f ~/dotfiles/vendor/nvm/nvm.sh && -d $NVM_DIR ]]; then
  source ~/dotfiles/vendor/nvm/nvm.sh
  nvm use
fi
