alias cuke='bundle exec cucumber -r features'

alias be="bundle exec"
alias bi="bundle install"
alias bl="bundle list"
alias bu="bundle update"
alias bco='bundle console'

alias rfo="rubocop --format offenses"
alias ro="rubocop --display-cop-names --format simple --only"

alias zs='zeus start'
alias zt='zeus test'
alias zco='zeus console'
alias zr='zeus rake'

alias rtags='ctags -R -f .tags --languages=ruby app lib'

rerubies () {
  RUBIES=(~/.rubies/*)
}
