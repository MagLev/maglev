module Kernel

  primitive_nobridge '_resolve_smalltalk_class', 'resolveSmalltalkClass:'

  # Print messages for stubbed methods
  @@gs_WARNSTUB = false
  def _stub_warn(msg)
    puts "== WARN: STUB: MNI: #{msg}" if @@gs_WARNSTUB
  end

  def load(name)
    RUBY.load(Type.coerce_to(name, String, :to_str))
  end

  def at_exit
    _stub_warn("Kernel#at_exit")
  end

  # See ruby-core:20222 for a discussion on whether to use Kernel#require
  # or some other private implementation for autoload.
  primitive_nobridge 'autoload', 'rubyKernelAutoload:file:'
  primitive_nobridge 'autoload?', 'rubyKernelAutoloadFileFor:'

  # Kernel#autoload?: STUB: Always returns nil.
  def autoload?(name)
    _stub_warn('Kernel#autoload?: always returns nil')
    nil
  end

  # binding defined in Kernel2.rb

  primitive_nobridge '_last_dnu_protection', '_lastDnuProtection'

  def method_missing(method_id, *args)
    prot = _last_dnu_protection()
    if (prot.equal?(0))
      raise NoMethodError, "Undefined method `#{method_id}' for #{self}  "
    elsif (prot.equal?(1))
      raise NoMethodError, "protected method `#{method_id}' called for  #{self}"
    else
      raise NoMethodError, "private method `#{method_id}' called for  #{self}"
    end
  end

  def caller(skip=0, limit=1000)
    # returns an Array of Strings, each element describes a stack frame
    unless skip._isFixnum
      raise ArgumentError
    end
    res = Thread._backtrace(false, limit)
    if (skip > 0)
      res = res[skip, res.length]
    end
    res
  end

  # def catch(aSymbol, &aBlock); end
  primitive_nobridge 'catch&' , 'catch:do:'

  primitive_nobridge '_eval', '_eval:binding:with:'

  def eval(str, binding, file_not_used, line_not_used)
    # use 0x3? because one extra stack frame due to bridging methods .
    # max send site is :::* , call is via a :::* to :::: bridge meth .
    vcgl = [ self._getRubyVcGlobal(0x30) ,
      self._getRubyVcGlobal(0x31) , nil ]
    res = _eval(str, binding, vcgl )
    vcgl[0]._storeRubyVcGlobal(0x30)
    vcgl[1]._storeRubyVcGlobal(0x31)
    res
  end

  def eval(str)
    # no bridge methods for this an subsequent variants
    vcgl = [ self._getRubyVcGlobal(0x20) ,
             self._getRubyVcGlobal(0x21) , self ]
    res = _eval(str, nil, vcgl )
    vcgl[0]._storeRubyVcGlobal(0x20)
    vcgl[1]._storeRubyVcGlobal(0x21)
    res
  end

  def eval(str, binding)
    vcgl = [ self._getRubyVcGlobal(0x20) ,
      self._getRubyVcGlobal(0x21), nil ]
    res = _eval(str, binding, vcgl )
    vcgl[0]._storeRubyVcGlobal(0x20)
    vcgl[1]._storeRubyVcGlobal(0x21)
    res
  end

  def eval(str, binding, file_not_used)
    vcgl = [ self._getRubyVcGlobal(0x20) ,
      self._getRubyVcGlobal(0x21) , nil ]
    res = _eval(str, binding, vcgl )
    vcgl[0]._storeRubyVcGlobal(0x20)
    vcgl[1]._storeRubyVcGlobal(0x21)
    res
  end

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

  primitive_nobridge 'include',  'includeRubyModule:'

  def loop
    raise LocalJumpError, "no block given" unless block_given?

    while true
      yield
    end
  end

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

  # def proc ...  in Kernel2.rb

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

  # def rand #  implemented in Kernel2.rb

  def raise(ex_class, message)
    ex = ex_class.exception
    ex.signal(message)
  end

  def raise(ex_class, message, *args)
    # TODO args is callback info not yet implemented
    raise(ex_class, message)
  end

  def raise(msg)
    if msg._isString
      raise(RuntimeError, msg)
    else
      # msg should be a subclass of Exception or
      #  an object that returns a new exception
      ex = msg.exception
      ex.signal
    end
  end

  def _reraise(ex)
    # _reraise invoked from IR generated for RubyVCallRaiseNode 
    ex._reraise
  end

  def raise
    #  if $! is valid in the caller, the parser will
    #   translate raise to  _resignal
    RuntimeError.signal
  end

  # sleep behavior
  # PickAxe book   says argument of zero means infinite sleep
  # MRI behavior,  sleep(0) returns immediately
  # Maglev, sleep(0) will be equivalent to Thread.pass

  primitive_nobridge '_sleep_ms', '_highPriorityWaitForMilliseconds:'

  def sleep(numeric=0)
    # returns number of seconds slept
    if numeric._isInteger 
      ms = numeric * 1000
    elsif 
      ms = (1000.0 * numeric).to_i 
    end
    ms_slept = _sleep_ms(ms) 
    ms_slept / 1000
  end

  def sleep_ms(milliseconds=0)
    # Gemstone addition
    # returns number of milliseconds slept
    ms = milliseconds
    unless milliseconds._isInteger 
      ms = milliseconds.to_i 
    end
    _sleep_ms(ms) 
  end

  def split(pattern=nil, limit=nil)
    $_ .split(pattern, limit)
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

  def trap(signal, proc)
    _stub_warn("Kernel#trap(signal, proc)")
  end

  def trap(signal, &block)
    _stub_warn("Kernel#trap(signal, &block)")
  end

  # def throw(aSymbol); end
  primitive_nobridge 'throw' , 'throw:'

  # def throw(aSymbol, aValue); end
  primitive_nobridge 'throw' , 'throw:with:'

  # This is a hook into the mspec framework so that the --spec-debug action
  # can call us.  E.g., to debug a failing spec, do:
  #
  #   spec/mspec/bin/mspec -T -d --spec-debug -K fails \
  #         spec/rubyspec/1.8/core/string/append_spec.rb
  def debugger
    nil.pause
  end

  def require(name)
    RUBY.require(Type.coerce_to(name, String, :to_str))
  end
end
