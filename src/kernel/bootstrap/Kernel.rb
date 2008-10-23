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

  primitive 'format*', 'sprintf:with:'

  def open(fName)
    File.open(fName)
  end

  def open(fName, mode)
    File.open(fName, mode)
  end

  def p(obj)
    f = STDOUT
    f.write(obj.inspect)
    f.write("\n")   # TODO observe record sep global
    nil
  end
 
  def print(*args)
    STDOUT.print(*args)
    nil
  end

  def printf(a, b, c, *d)
    if (a.kind_of?(IO))
      if (d._isArray)
        args = [ c ]
        args.concat(*d)
      else
        args = [ c , d ]
      end
      a.printf(b, *args)
    else
      if (d._isArray)
        args = [ b, c ]
        args.concat(*d)
      else
        args = [ b, c , d ]
      end
      STDOUT.printf(a, *args)
    end
  end

  def printf(a, b, c)
    if (a.kind_of?(IO))
      a.printf(b, c)
    else
      STDOUT.printf(a, b, c)
    end
  end

  def printf(a, b)
    if (a.kind_of?(IO))
      a.printf(b)
    else
      STDOUT.printf(a, b)
    end
  end

  def printf(a)
    STDOUT.printf(a)
  end

  def puts(*args)
    STDOUT.puts(*args)
    nil
  end

  def putc(arg)
    STDOUT.putc(arg)
    arg
  end


  primitive 'sprintf*', 'sprintf:with:'

  primitive_nobridge '_system', '_system:'

  def system(command, *args)
    cmd = command 
    n = 0
    sz = args.length
    while n < sz
      if (n < sz - 1)
        cmd << ' ' 
      end
      cmd << args[n].to_s
      n = n + 1
    end 
    resultStr = Kernel._system(cmd)
    if (resultStr)
      puts resultStr
      return true
    end
    return false
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
