namespace :sake do
  desc "Pushes local source files to madbytes.net -- no need to run this."
  task :push do
    source = ENV['TASK_SOURCE'] || File.join(ENV['HOME'], 'hg', 'sake')
    files = Dir[File.join(source, '*.rake')]
    tasks = []
    files.each do |f|
      task = File.basename(f).sub(/\.rake$/, '')
      tasks << task
      sh "scp #{f} root@web:/var/www/vhosts/madbytes.net/httpdocs/sake/#{task}.rake"
    end

    tmpfile = `mktemp /tmp/sakeXXXXXXXXXXX`.chomp
    File.open(tmpfile, 'w') { |f| f.puts tasks.join("\n") }
    sh "scp #{tmpfile} root@web:/var/www/vhosts/madbytes.net/httpdocs/sake/tasks.list"
    sh "rm #{tmpfile}"
  end

  desc 'Reloads sake by pulling a fresh copy of the sources from madbytes.net'
  task :pull do
    require 'open-uri'
    sh "rm #{File.join(ENV['HOME'], '.sake')}"
    tasks = open("http://madbytes.net/sake/tasks.list").read.split("\n")
    tasks.each { |task| sh "sake -i http://madbytes.net/sake/#{task}.rake" }
  end

  desc "Reloads sake from my local copies -- no need to run this."
  task :reload do
    sh "rm #{File.join(ENV['HOME'], '.sake')}"
    Dir[File.join(ENV['HOME'], 'hg', 'sake', '*.rake')].each do |f|
      sh "sake -i #{f}"
    end
  end
end
