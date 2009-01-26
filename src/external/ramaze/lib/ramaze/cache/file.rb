module Ramaze
  # Persist cache contents to the filesystem.
  # By default this will create a `/cache` directory in your APPDIR
  #
  # Usage for sessions only:
  #
  #     Ramaze::Global::cache_alternative[:sessions] = Ramaze::FileCache
  #
  # Usage for everything:
  #
  #     Ramaze::Global::cache = Ramaze::FileCache

  class FileCache
    attr_accessor :root, :subdir
    attr_reader :host, :pid

    def initialize(root = Ramaze::Global.root, subdir = 'cache')
      @root, @subdir = root, subdir
      @host = Socket.gethostname
      @pid = $$

      FileUtils.mkdir_p(dir)
    end

    def dir(*further)
      File.join(root, subdir, *further)
    end

    def [](key)
      Marshal.load(File.read(dir(key.to_s, 'data')))
    rescue
      nil
    end

    def []=(key, value)
      key = key.to_s
      tmp_name = dir(key, "data.#{host}.#{pid}")
      key_name = dir(key, 'data')
      dir_name = dir(key)

      data = Marshal.dump(value)

      FileUtils.rm_rf(dir_name)
      FileUtils.mkdir_p(dir_name)

      File.open(tmp_name, 'w'){|fd| fd.write(data) }

      FileUtils.mv(tmp_name, key_name)

      return value
    end

    def values_at(*keys)
      keys.map{|key| self[key] }
    end

    def delete(*keys)
      keys.map do |key|
        FileUtils.rm_rf(dir(key.to_s))
      end
    end

    def clear
      Dir[dir('*')].each{|entry| FileUtils.rm_rf(entry) }
    end

    def to_sym
      name.split('::').last.to_sym
    end
  end
end
