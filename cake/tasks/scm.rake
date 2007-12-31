require 'repository'

namespace :scm do
  task :st do
    Dir['*'].each do |dir|
      dir = File.expand_path(dir)
      repo = Repository.new(dir)
      puts "Repository: #{repo.name} [#{repo.type}]"
      puts "  rev: #{repo.head_revision}"
      puts
    end
  end
end
