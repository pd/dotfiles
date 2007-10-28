# Posted by Dr. Nic
# http://pastie.caboo.se/76167.txt
namespace :gems do
  desc 'Return path to a gem in cache; e.g. sake gems:find activerecord | xargs mate'
  task :find do
    if ARGV.last
      gem_path = Gem.source_index.find_name(ARGV.last).last.full_gem_path
    else
      gem_path = Gem.source_index.find_name('sources').last.full_gem_path.split('sources').first
    end

    print gem_path
  end
end
