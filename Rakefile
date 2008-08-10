# I don't care about most dotfiles on remote systems;
# these define the ones that will be installed by the :install task.
# Anything else I probably only want on my own machine, and
# I can do it manually.
IMPORTANT_FILES = %w(
  gitconfig
  gitignore
  inputrc
  irbrc
  screenrc
  vim
  vimrc
  zsh
  zshrc
)

task :install do
  repo = File.expand_path(File.dirname(__FILE__))
  home = File.expand_path('~')

  IMPORTANT_FILES.each do |fn|
    realfile = File.join(repo, fn)
    dotfile  = File.join(home, ".#{fn}")
    if File.exist?(dotfile)
      puts "Skipping #{dotfile}; already exists."
    else
      FileUtils.ln_s(realfile, dotfile)
    end
  end
end
