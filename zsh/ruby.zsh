## use gems from ~/.gem or ~/gems if present
if [ -d ~/.gem ]; then
  export GEM_HOME=~/.gem
  path=(~/.gem/bin $path)
elif [ -d ~/gems ]; then
  export GEM_HOME=~/gems
  path=(~/gems/bin $path)
fi

## rip
if [ -d ~/.rip ]; then
  export RIPDIR=~/.rip
  export RUBYLIB=$RIPDIR/active/lib
  path=(~/.rip/active/bin $path)
fi
