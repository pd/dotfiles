## generic shortcuts
alias att='autotest'

## use gems from ~/gems, easier to maintain
export GEM_HOME=~/gems
path=(~/gems/bin $path)

## rails / merb
## uses the rails command if it's present, otherwise merb
alias ss='./script/spec -O spec/spec.opts'
alias sse='./script/server'
alias sc='./script/console'
alias sg='./script/generate'
alias edge-rails='ruby ~/vendor/rails/railties/bin/rails'

## launch textmate using the cwd as the project root
mt () {
  [ "$PWD" = "$HOME" ] && echo "not in $HOME." && return
  mate .
}
