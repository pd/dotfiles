require 'repository'

namespace :scm do
  task :st do
    Dir['*'].map { |d| File.expand_path(d) }.each do |dir|
      repo = Repository.new(dir)
      puts "Repository: #{repo.name} [#{repo.type}]"
      puts "  rev: #{repo.head_revision}"
      puts
    end
  end
end
