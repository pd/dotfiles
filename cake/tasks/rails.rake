namespace :rails do
  desc 'Ensures the ~/rails/projects directory is available'
  task :create_project_base_dir do
    project_base = ENV['RAILS_PROJECT_BASE'] || File.join(ENV['HOME'], 'rails', 'projects')
    sh "mkdir -p #{project_base}" if !File.exist?(project_base)
    raise "#{project_base} is in the way; should be a directory" unless File.directory?(project_base)
    ENV['RAILS_PROJECT_BASE'] = project_base
  end

  desc 'Ensures a project name is in the "project" environment variable'
  task :project_name do
    unless ENV['project']
      raise "Pass the project name using project='myapp' in the environment"
    end
  end

  desc 'Ensures a task is being run inside of a rails project'
  task :verify_in_root do
    %w(app config db vendor).each { |d| raise "Not in Rails project root?" unless File.directory?(d) }
  end

  desc 'Runs the db:create rake task for all defined environments'
  task :create_db => 'rails:verify_in_root' do
    sh "rake db:create:all"
  end

  desc 'Copies the example db.yml into place as the real one'
  task :dbyml => 'rails:verify_in_root' do
    sh "cp config/database.yml.example config/database.yml"
  end

  desc 'Generates a new Rails project'
  task :generate => ['rails:create_project_base_dir', 'rails:project_name'] do
    project = ENV['project']

    edge_location = File.join(ENV['HOME'], 'vendor', 'rails')
    generator = File.join(edge_location,  'railties', 'bin', 'rails')
    unless File.exist?(generator)
      raise "Can't generate a project without edge rails checked out in #{edge_location}"
    end

    if File.exist?(File.join(ENV['RAILS_PROJECT_BASE'], project))
      raise "Project #{project} already exists?"
    end
    Dir.chdir ENV['RAILS_PROJECT_BASE']
    sh "ruby #{generator} #{project}"
  end

  desc 'Sets up a project for use with Mercurial'
  task :hg_init => 'rails:verify_in_root' do
    raise "Mercurial repository already exists here" if File.exist?('.hg')

    # initial setup
    sh "hg init ."
    sh "hg add *"
    sh "hg ci -m 'initialize new rails project'"

    # placeholder files
    %w(cache pids sessions sockets).each do |d|
      d = "tmp/#{d}/.placeholder"
      sh "touch #{d}"
      sh "hg add #{d}"
    end
    %w(log lib/tasks).each do |d|
      d = "#{d}/.placeholder"
      sh "touch #{d}"
      sh "hg add #{d}"
    end
    sh "hg ci -m 'add placeholder files'"

    # move database.yml out of the way
    sh "hg mv config/database.yml config/database.yml.example"
    sh "hg ci -m 'move database.yml out of the way'"

    # ignore volatile files
    File.open('.hgignore', 'w') do |f|
      f.puts <<-EOF.gsub(/^\s+/, '')
        ^log/
        ^tmp/
        ^config/database.yml$
        ^db/.*.db$
        ^db/schema.rb$
        ^coverage/
      EOF
    end
    sh "hg add .hgignore"
    sh "hg ci -m 'ignore volatile files'"

    # remove log files
    Dir['log/*.log'].each { |file| sh "hg rm #{file}" }
    sh "hg ci -m 'remove log files'"
  end

  desc 'Installs/updates to Rails edge, rspec+rspec_on_rails trunk, commits changes to hg'
  task :edge => 'rails:verify_in_root' do
    sh "rake rails:freeze:edge"
    revision = `ls vendor/rails/REVISION_*`.sub(/.*REVISION_(\d+)/, '\1').chomp
    sh "hg st -nu vendor/rails | xargs -L 256 hg add"
    sh "hg st -nd vendor/rails | xargs -L 256 hg rm"
    sh "hg ci -m 'freeze to rails edge r#{revision}' vendor/rails"

    svn_url = "svn://rubyforge.org/var/svn/rspec/trunk"
    %w(rspec rspec_on_rails).each do |plugin|
      sh "rm -rf vendor/plugins/#{plugin}"
      rev = `svn export #{svn_url}/#{plugin} vendor/plugins/#{plugin}`.sub(/.*Exported revision (\d+)\./m, '\1').chomp
      sh "hg st -nu vendor/plugins/#{plugin} | xargs -L 256 hg add"
      sh "hg st -nu vendor/plugins/#{plugin} | xargs -L 256 hg rm"
      sh "hg ci -m 'freeze to #{plugin} r#{rev}' vendor/plugins/#{plugin}"
    end
  end

  desc 'Bootstraps rspec'
  task :bootstrap => ['rails:verify_in_root', 'rails:dbyml'] do
    raise "rspec already bootstrapped!" if File.directory?('spec')
    sh "ruby script/generate rspec"
    sh "rm previous_failures.txt"
    sh "hg add spec/ script/spec*"
    sh "hg ci -m 'bootstrap rspec'"
  end

  desc 'Performs the tedious early setup of new Rails projects'
  task :setup => ['rails:verify_in_root', 'rails:dbyml'] do
    # AR as session store
    sh "sed -e 's/# \\(config.action_controller.session_store =\\)/\\1/' -i~ config/environment.rb"
    sh "rake db:sessions:create"
    sh "rake db:migrate"
    sh "hg add db/migrate"
    sh "hg ci -m 'use ActiveRecord as the session store' config/environment.rb db/migrate"

    # UTC
    sh "sed -e 's/# \\(config.active_record.default_timezone =\\)/\\1/' -i~ config/environment.rb"
    sh "hg ci -m 'use UTC as the default timezone' config/environment.rb"

    # vendor/gems
    sh %(sed -e "s,# config.load_paths += %W.*,config.load_paths += Dir[\\"\#{RAILS_ROOT}/vendor/gems/**\\"].map { |d| File.directory?(lib = \\"\#{d}/lib\\") ? lib : dir }," -i~ config/environment.rb)
    sh "hg ci -m 'allow loading of gems from vendor/gems' config/environment.rb"

    # database connection timeouts
    sh "echo ActiveRecord::Base.verification_timeout = 14400 >> config/environments/production.rb"
    sh "hg ci -m 'set ActiveRecord verification timeout for production environment' config/environments/production.rb"

    # cleanup
    sh "rm config/environment.rb~"
  end

  desc 'Capification'
  task :capify => 'rails:verify_in_root' do
    sh "capify ."
    sh "hg add Capfile config/deploy.rb"
    sh "hg ci -m 'capify' Capfile config/deploy.rb"

    sh "curl http://madbytes.net/configs/mongrel_cluster.yml > config/mongrel_cluster.yml"
    sh "hg add config/mongrel_cluster.yml"
    sh "hg ci -m 'add a generic, incomplete mongrel cluster configuration' config/mongrel_cluster.yml"

    sh "curl http://madbytes.net/configs/mongrel_cluster_recipe.rb > config/mongrel_cluster_recipe.rb"
    sh "hg add config/mongrel_cluster_recipe.rb"
    sh "hg ci -m 'add mongrel cluster support for cap2' config/mongrel_cluster_recipe.rb"

    sh "curl http://madbytes.net/configs/deploy.rb > config/deploy.rb"
    sh "hg ci -m 'skeletal deployment recipe for mercurial and mongrel cluster' config/deploy.rb"
  end

  desc 'Magical task to prepare a brand-new Rails project'
  task :create => ['rails:create_project_base_dir', 'rails:project_name'] do
    project = ENV['project']
    sh "sake rails:generate project='#{project}'"
    Dir.chdir File.join(ENV['RAILS_PROJECT_BASE'], project)
    %w(hg_init edge bootstrap create_db setup capify).each { |task| sh "sake rails:#{task}" }
  end

  desc 'Prepares the 3-dir project setup'
  task :clone => ['rails:create_project_base_dir', 'rails:project_name'] do
    project = ENV['project']
    base = File.join(ENV['RAILS_PROJECT_BASE'], project)
    raise "#{base} is in the way" if File.exist?(base)

    sh "mkdir -p #{base}"
    Dir.chdir base

    sh "hg clone ssh://hg/#{project} staging"
    sh "hg clone staging/ work/"
    sh "hg clone staging/ deploy/"
    File.open('deploy/.hg/hgrc', 'w') do |f|
      f.puts <<-EOF.gsub(/^\s+/, '')
        [paths]
        default = ssh://hg/#{project}
        default-push = ssh://hg@rails//usr/local/hg/#{project}
      EOF
    end
  end
end
