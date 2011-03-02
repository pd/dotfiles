if [[ -f ~/vendor/nvm/nvm.sh ]]; then
  NVM_DIR=~/.nvm
  [[ ! -d $NVM_DIR ]] && mkdir $NVM_DIR
  source ~/vendor/nvm/nvm.sh
  nvm use default >/dev/null
fi
