namespace :vendor do
  desc 'Sets the vendor root path as VENDOR_ROOT in the environment'
  task :set_root do
    ENV['VENDOR_ROOT'] ||= File.join(ENV['HOME'], 'vendor')
  end

  desc 'Pushes the list of projects to madbytes.net'
  task :push => 'vendor:set_root' do
    tmpfile = `mktemp /tmp/sakevendorXXXXXXXXXX`.chomp
    File.open(tmpfile, 'w') do |f|
      Dir[File.join(ENV['VENDOR_ROOT'], '*')].each do |dir|
        name = File.basename(dir)
        scm, ucmd = nil, nil
        if File.directory?(File.join(dir, '.hg'))
          scm  = 'hg'
          ucmd = "hg paths | grep ^default | awk '{print $3}'"
        elsif File.directory?(File.join(dir, '.git'))
          scm  = 'git'
          ucmd = "git remote show origin | grep URL | awk '{print $2}'"
        elsif File.directory?(File.join(dir, '.svn'))
          scm  = 'svn'
          ucmd = "svn info | grep URL | awk '{print $2}'"
        else
          next
        end
        Dir.chdir dir
        url = `#{ucmd}`
        f.puts "#{scm} #{name} #{url}"
      end
    end
    sh "scp #{tmpfile} root@lwweb:/www/madbytes.net/httpdocs/sake/vendor.projects.list"
    sh "rm #{tmpfile}"
  end

  desc 'Checks out a bunch of projects into ~/vendor'
  task :co => 'vendor:set_root' do
    require 'open-uri'
    sh "mkdir -p #{ENV['VENDOR_ROOT']}" if !File.exist?(ENV['VENDOR_ROOT'])
    projects = open('http://madbytes.net/sake/vendor.projects.list').read.split("\n")
    Dir.chdir ENV['VENDOR_ROOT']
    projects.each do |project|
      scm, name, url = project.split(' ', 3)
      if !File.exist?(name)
        case scm
        when 'hg'  then cmd = "hg clone #{url} #{name}"
        when 'git' then cmd = "git clone #{url} #{name}"
        when 'svn' then cmd = "svn co #{url} #{name}"
        else
          puts "Skipping project #{name}, unrecognized SCM #{scm}"
          next
        end
        puts "Checking out #{name} from #{url}"
        puts cmd
        puts `#{cmd}`
        puts
      else
        puts "Skipping project #{name}, it already exists"
      end
    end
  end

  desc 'Updates all of the projects in ~/vendor'
  task :up => 'vendor:set_root' do
    Dir[File.join(ENV['VENDOR_ROOT'], '*')].each do |dir|
      if File.directory?(File.join(dir, '.hg'))
        cmd = "hg pull -u"
      elsif File.directory?(File.join(dir, '.git'))
        cmd = "git pull"
      elsif File.directory?(File.join(dir, '.svn'))
        cmd = "svn up"
      else
        next
      end
      Dir.chdir dir
      puts "Updating #{File.basename(dir)}"
      puts `#{cmd}`
      puts
    end
  end
end
