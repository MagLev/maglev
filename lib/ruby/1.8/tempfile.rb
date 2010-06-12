# maglev variant on tempfile.rb
# implement Tempfile as a subclass of File rather than using DelegateClass(File),
#  so that vcGlobal dependent implementations can be inherited properly, etc.

require 'tmpdir'

class Tempfile < File
  MAX_TRY = 10
  @@cleanlist = []

  def self.make_tmpname(basename, n)
    if basename._isArray
      prefix, suffix = *basename
    else
      prefix, suffix = basename, ''
    end

    t = Time.now.strftime("%Y%m%d")
    path = "#{prefix}#{t}-#{$$}-#{rand(0x100000000).to_s(36)}-#{n}#{suffix}"
  end

  def self.open(*args, &block)
    tempfile = self.new(*args)
    if block_given?
      begin
	block.call(tempfile)
      ensure
	tempfile.__close
      end
      nil
    else
      tempfile
    end
  end

  def self.new(basename, tmpdir=Dir::tmpdir)
    lock = nil
    n = failure = 0
    begin
      Thread.critical = true

      begin
        tmpname = File.join(tmpdir, make_tmpname(basename, n))
        lock = tmpname + '.lock'
        n += 1
      end while @@cleanlist.include?(tmpname) or
        File.exist?(lock) or File.exist?(tmpname)

      Dir.mkdir(lock)
    rescue
      failure += 1
      retry if failure < MAX_TRY
      raise "cannot generate tempfile `%s'" % tmpname
    ensure
      Thread.critical = false
    end

    begin
      inst = self.__create(tmpname, File::RDWR|File::CREAT|File::EXCL, 0600)
      @@cleanlist << tmpname
      # maglev: The proc need not be reachable from the instance,
      # finalizers list will keep it alive.  The proc takes the
      # Object to be finalized as it's one arg .
      cl_proc = Proc.new { | a_tempfile | a_tempfile.__close }
      ObjectSpace.define_finalizer( inst, cl_proc )
    ensure
      Dir.rmdir(lock)
    end
    inst
  end

  def close(unlink_now=false)
    if unlink_now
      close!
    else
      __close
    end
  end

  def close!
    self.__close_and_delete
    ObjectSpace.undefine_finalizer(self)
  end

  def _close   # for spec compatibility
   self.__close_and_delete
  end
  protected :_close

  def __close_and_delete
    mypath = self.path
    begin
      if self.__is_open
        self.__close
      end 
      File.unlink(mypath)
    rescue
      # ignore
    end
    @@cleanlist.delete(mypath)
  end

  # Opens or reopens the file with mode "r+".
  def open
    path = self.path
    if self.__is_open
      self.__close
    end
    f = self.__fopen(path, "r+")
    Errno.raise_errno(f, path ) if f._isFixnum
    self
  end

  def size
    if self.closed?
      return 0
    end
    self.stat.size
  end
  alias length size

  def unlink
    # keep this order for thread safeness
    begin
      tmpname = self.path
      File.unlink(tmpname) if File.exist?(tmpname)
      @@cleanlist.delete(tmpname)
      ObjectSpace.undefine_finalizer(self)
    rescue Errno::EACCES
      # may not be able to unlink on Windows; just ignore
    end
  end
  alias delete unlink

  # everything else inherited from File

end
