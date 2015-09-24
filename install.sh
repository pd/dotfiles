#!/usr/bin/env bash

set -eu

DOTDIR="${HOME}/dotfiles"
BINDIR="${HOME}/bin"

noisy_link () {
  echo "ln -s \"${1}\" \"${2}\""
  ln -s "${1}" "${2}"
}

cautious_link () {
  if [[ -e "${2}" ]]; then
    echo "skipped: ${2}"
  else
    noisy_link "${1}" "${2}"
  fi
}

cautious_link_bin () {
  local name=$(basename "$1")
  local src="${DOTDIR}/bin/${name}"
  local dest="${BINDIR}/${name}"

  if [[ ! -d "${src}" ]]; then
    cautious_link "${src}" "${dest}"
  fi
}

install_vim_vundle () {
  git clone git://github.com/VundleVim/VundleVim.vim.git ~/.vim/bundle/Vundle.vim
}

for f in zshrc zshenv emacs.d gitconfig vim vimrc vim-tmp pryrc irbrc sqliterc psqlrc; do
  cautious_link "${HOME}/dotfiles/${f}" "${HOME}/.${f}"
done

for x in ./bin/*; do
  cautious_link_bin "${x}"
done

install_vim_vundle
