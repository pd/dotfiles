namespace :hg do
  desc 'Display status of mercurial repositories in current directory'
  task :status do
    dirs = Dir['*'].map { |d| File.expand_path(d) }
    dirs.each do |dir|
      repo = File.basename(dir)
      next unless File.directory?(dir) && File.directory?(File.join(dir, '.hg'))
      Dir.chdir dir
      status = `hg status`.chomp
      puts "Repository: #{repo}/"
      puts "  rev #{`hg head --template '{rev}'`}"
      puts status == '' ? 'No changes.' : status
      puts
    end
  end

  desc 'Alias for hg:status'
  task :st => 'hg:status'
end
