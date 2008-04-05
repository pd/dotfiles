## generic shortcuts
alias gs='gem server'
alias att='autotest'

## use gems from ~/gems, easier to maintain
export GEM_HOME=~/gems
path=(~/gems/bin $path)

## rails / merb
## uses the rails command if it's present, otherwise merb
sse () {
  ([ -f './script/server' ] && ruby ./script/server $*) || merb $*
}
sc () {
  ([ -f './script/console' ] && ruby ./script/console $*) || merb -i $*
}
sg () {
  ([ -f './script/generate' ] && ruby ./script/generate $*) || merb-gen $*
}
sst () {
  ([ -f './script/story' ] && ruby ./script/story $*) || rake 'story[all]'
}
alias edge-rails='ruby ~/vendor/rails/railties/bin/rails'

## launch textmate using the cwd as the project root
mt () {
  [ "$PWD" = "$HOME" ] && echo "not in $HOME." && return
  mate .
}
