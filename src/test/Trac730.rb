# From Rails

def get_random_string
  flags = File::RDONLY
  flags |= File::NONBLOCK if defined? File::NONBLOCK
  flags |= File::NOCTTY if defined? File::NOCTTY
  flags |= File::NOFOLLOW if defined? File::NOFOLLOW
  n = 16
  begin
    File.open("/dev/urandom", flags) {|f|
      unless f.stat.chardev?
        raise Errno::ENOENT
      end
      ret = f.readpartial(n)
      if ret.length != n
        raise NotImplementedError, "Unexpected partial read from random device"
      end
      return ret
    }
  end
end

p get_random_string

raise 'fail' unless get_random_string.length == 16
