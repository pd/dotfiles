NVM_DIR=~/.nvm
if [[ -f ~/dotfiles/vendor/nvm/nvm.sh && -d $NVM_DIR ]]; then
  echo "loading nvm"
  source ~/dotfiles/vendor/nvm/nvm.sh
fi
