module Kernel
  # Object has an empty module Kernel installed as its superclass
  #   during the slowrubyimage step of server build.
  #  this file will extend Kernel by adding methods to it.

  # following methods are just those needed to get some benchmarks and
  #   specs running .

  def exit(arg=1)
    status = '9'
    if (arg.equal?(true))
      status = '0'
    elsif (arg._isInteger)
      status = arg.to_s
    end
    raise SystemExit , status
  end

  def format(str, *args)
    args.each{|a| str.sub!(/%(d|s)/, a.to_s)}
    str
  end

  def open(fName)
    File.open(fName)
  end

  def open(fName, mode)
    File.open(fName, mode)
  end

  def taint
    # TODO: Kernel#taint is a noop!
  end

  def tainted?
    false # TODO kernel#taint is a stub: return false
  end

  def untaint
    # TODO: Kernel#untaint is a noop!
  end
end
