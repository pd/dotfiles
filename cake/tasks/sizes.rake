require 'pp'

namespace :dir do
  task :sizes do
    sizes = `du -sk *`.split("\n").map { |ln| tks = ln.chomp.split("\t"); [tks[0].to_i, tks[1]] }
    pp sizes.sort { |a, b| b[0] <=> a[0] }
  end
end
