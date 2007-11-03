class Repository
  attr_reader :path, :name, :type

  def initialize(path)
    if !File.exist?(path) || !File.directory?(path)
      raise ArgumentError, "error reading #{path}"
    end
    @path = path
    @name = File.basename(path)
    @type = determine_type(@path)
  end

  def determine_type(path)
    if File.directory? File.join(path, '.hg')
      'hg'
    elsif File.directory? File.join(path, '.git', 'svn')
      'git-svn'
    elsif File.directory? File.join(path, '.git')
      'git'
    elsif File.directory? File.join(path, '.svn')
      'svn'
    else
      raise ArgumentError, "unknown repository type in #{path}"
    end
  end

  def head_revision
    @head_revision ||= begin
      if type == 'hg'
        in_repo { `hg head --template '{rev}'` }
      elsif type == 'git'
        in_repo { `git-show --stat --abbrev-commit HEAD | sed -n 's/^commit //p'` }
      elsif type == 'svn'
        in_repo { `svn info | sed -n 's/^Revision: //p'` }
      else
        'dunno yet'
      end
    end
  end

  private
    def in_repo
      cwd = Dir.pwd
      Dir.chdir path
      rval = yield
      Dir.chdir cwd
      rval
    end
end
