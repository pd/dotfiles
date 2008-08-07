namespace :git do
  task :gc do
    Dir['*'].each do |dir|
      Repository.new(dir).gc
    end
  end
end
