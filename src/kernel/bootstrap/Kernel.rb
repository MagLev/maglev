module Kernel

  # following methods are just those needed to get some benchmarks and
  # specs running .

  # Kernel#autoload: STUB: This stubbed version just calls +require
  # file_name+ rather than defering the require.
  def autoload(name, file_name)
    require file_name
    nil
  end

  # Kernel#autoload?: STUB: Always returns nil.
  def autoload?(name)
    nil
  end

  # TODO: Currently, smalltalk "@ruby:method_missing" doesn't allow you to
  # pass multiple parameters, so we insert _method_missing to repackage the
  # params. doesNotUnderstand calls _method_missing with a single array
  # containing the method_id followed by any parameters. This is probably
  # faster than doing a smalltalk perform from Object>>doesNotUnderstand.
  def _method_missing(args)
    # args is an array guaranteed to have args[0] be a symbol.
    method_missing(*args)
  end

  def method_missing(method_id, *args)
    raise NoMethodError, "Undefined method `#{method_id}' for #{self}"
  end

  # def catch(aSymbol, &aBlock); end
  primitive_nobridge 'catch' , 'catch:do:'

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
    if STDOUT.nil?
      raise "STDOUT is nil in Kernel.puts!"
    else
      STDOUT.puts(*args)
    end
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

  # def throw(aSymbol); end
  primitive_nobridge 'throw' , 'throw:'

  # def throw(aSymbol, aValue); end
  primitive_nobridge 'throw' , 'throw:with:'

end
