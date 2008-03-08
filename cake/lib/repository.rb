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
    elsif File.directory? File.join(path, 'CVS')
      'cvs'
    else
      '?'
    end
  end

  def head_revision
    @head_revision ||= begin
      if type == 'hg'
        in_repo { `hg head --template '{rev}'` }
      elsif type == 'git'
        in_repo do
          stat = `git-show --stat --abbrev-commit HEAD`
          stat.match(/^commit (\w+)\.\.\./).captures.first
        end
      elsif type == 'git-svn'
        in_repo do
          stat = `git-show --stat --abbrev-commit HEAD`
          gitrev = stat.match(/commit (\w+)\.\.\./).captures.first
          svnrev = stat.match(/git-svn-id: .*@(\d+)/).captures.first
          "#{gitrev} = r#{svnrev}"
        end
      elsif type == 'svn'
        in_repo { `svn info | sed -n 's/^Revision: //p'` }
      elsif type == 'cvs'
        'cvs-dun-work-like-that?'
      else
        'dunno yet'
      end
    end
  end

  def update
    case type
    when 'hg'
      in_repo { `hg pull -u` }
    when 'git-svn'
      in_repo { `git-svn rebase` }
    when 'git'
      in_repo { `git pull` }
    when 'svn'
      in_repo { `svn update` }
    when 'cvs'
      in_repo { `cvs -q update -dP` }
    else
      "can't update unknown repository type"
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
