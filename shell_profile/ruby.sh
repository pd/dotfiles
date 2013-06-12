alias cuke='bundle exec cucumber -r features'

alias be="bundle exec"
alias bi="bundle install"
alias bl="bundle list"
alias bu="bundle update"

alias zr='zeus rake'
alias zt='zeus test'
alias zdb='zeus dbconsole'
alias zco='zeus console'
alias zck='zeus cucumber'

dbr () {
  if command lunchy >/dev/null 2>&1; then
    lunchy stop pow
  fi

  bundle exec rake db:{drop,create,migrate,seed}

  if command lunchy >/dev/null 2>&1; then
    lunchy start pow
  fi
}
