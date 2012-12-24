desc "Install core, bin and desktop"
task :install => ['install:core', 'install:bin', 'install:desktop']

desc "Install zsh, emacs, git, etc"
task 'install:core' do
  files = %w[zshrc emacs.d gitconfig gitignore vim vimrc vim-tmp]
  files.each { |f| dotlink f }
end

desc "Install ~/bin"
task 'install:bin' do
  bins = %w[bin private/bin]
  bins.map { |bin| File.join Dir.pwd, bin }.each { |bin| binlink bin }
end

def link(src, dest)
  if File.exist? dest
    puts "## File #{dest} exists, skipping."
  else
    puts ">> #{src} -> #{dest}"
    FileUtils.ln_s src, dest
  end
end

def dotlink(name)
  src  = File.join Dir.pwd, name
  dest = File.expand_path "~/.#{name}"
  link src, dest
end

def binlink(bindir)
  bindir = File.expand_path bindir
  bin = File.expand_path "~/bin"
  ensure_bin bin

  Dir[File.join bindir, '*'].each do |src|
    name = File.basename src
    dest = File.join bin, name
    link src, dest
  end
end

def ensure_bin(dir)
  FileUtils.mkdir_p dir unless File.exist? dir
  unless File.directory? dir
    puts "!! Path #{bin} already exists, but is not a directory. Wtfmate."
    raise RuntimeError
  end
end
