module Kernel

  primitive_nobridge '_resolve_smalltalk_global', 'resolveSmalltalkGlobal:'

  # _smalltalk_global_put for use by bootstrap code only
  primitive_nobridge '_smalltalk_global_put', 'smalltalkUserGlobalsAt:put:'

  # Print messages for stubbed methods
  # presence of classvars disturbs specs, constants disturbs vmunit tests
  # MAGLEV_WARNSTUB = false
  # MAGLEV_SEEN = { }  # TODO, should be a transient hash

  def _stub_warn(msg)
    #if MAGLEV_WARNSTUB
    #  unless MAGLEV_SEEN[msg]
    #    puts "== WARN: STUB: MNI: #{msg}"
    #    MAGLEV_SEEN[msg] = 1
    #  end
    #end
  end

  def load(name)
    RUBY.load(Type.coerce_to(name, String, :to_str))
  end

  def abort(string)
    puts string
    exit(1)
  end

  primitive_nobridge '_at_exit', 'atExit:'

  def at_exit(&block)
    proc = Proc.new(&block)
    _at_exit(proc._block )
    proc
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

  primitive_nobridge 'at_exit&', 'atExit:'

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
  primitive_nobridge '_eval_with_position', '_eval:binding:with:fileName:lineNumber:'

  def eval(str, binding, file_name, line_number=1 )
    # use _binding_ctx(1) and 0x3? because one extra stack frame due to bridging methods .
    # max send site is :::* , call is via a :::* to :::: bridge meth .
    if binding.equal?(nil)
      ctx = self._binding_ctx(1)
      bnd = Binding.new(ctx, self, nil)
    else
      bnd = binding
      unless bnd.is_a?(Binding) ; raise TypeError,'not a Binding' ; end
    end
    vcgl = [ self._getRubyVcGlobal(0x30) ,
      self._getRubyVcGlobal(0x31) , nil ]
    blk = bnd.block
    unless blk.equal?(nil)
      vcgl << blk
    end
    res = _eval_with_position(str, bnd, vcgl, file_name, line_number )
    vcgl[0]._storeRubyVcGlobal(0x30)
    vcgl[1]._storeRubyVcGlobal(0x31)
    res
  end

  def eval(str)
    # no bridge methods for this and subsequent variants
    ctx = self._binding_ctx(0)
    bnd = Binding.new(ctx, self, nil)
    vcgl = [ self._getRubyVcGlobal(0x20) ,
             self._getRubyVcGlobal(0x21) , self ]
    blk = bnd.block
    unless blk.equal?(nil)
      vcgl << blk
    end
    res = _eval(str, bnd, vcgl )
    vcgl[0]._storeRubyVcGlobal(0x20)
    vcgl[1]._storeRubyVcGlobal(0x21)
    res
  end

  def eval(str, &blk)
    ctx = self._binding_ctx(0)
    bnd = Binding.new(ctx, self, nil)
    vcgl = [ self._getRubyVcGlobal(0x20) ,
             self._getRubyVcGlobal(0x21) , self ]
    unless blk.equal?(nil)
      vcgl << blk
    end
    res = _eval(str, bnd, vcgl )
    vcgl[0]._storeRubyVcGlobal(0x20)
    vcgl[1]._storeRubyVcGlobal(0x21)
    res
  end

  def eval(str, binding)
    if binding.equal?(nil)
      ctx = self._binding_ctx(0)
      bnd = Binding.new(ctx, self, nil)
    else
      bnd = binding
      unless bnd.is_a?(Binding) ; raise TypeError,'not a Binding' ; end
    end
    vcgl = [ self._getRubyVcGlobal(0x20) ,
      self._getRubyVcGlobal(0x21), nil ]
    blk = bnd.block
    unless blk.equal?(nil)
      vcgl << blk
    end
    res = _eval(str, bnd, vcgl )
    vcgl[0]._storeRubyVcGlobal(0x20)
    vcgl[1]._storeRubyVcGlobal(0x21)
    res
  end

  def exit(arg=1)
    status = 9
    if (arg.equal?(true))
      status = 0
    elsif (arg._isInteger)
      status = arg
    end
    raise SystemExit.new(status)
  end

  primitive 'format*', 'sprintf:with:'

  primitive 'global_variables', 'rubyGlobalVariables'

  # This implementation of include handles include from a main program
  primitive_nobridge '_include_module',  'includeRubyModule:'
  def include(*names)
    # this variant gets bridge methods
    names.reverse.each do |name|
      _include_module(name)
    end
  end
  def include(name)
    # variant needed for bootstrap
    _include_module(name)
  end

  # def loop(&block) ; end
  primitive 'loop&', '_rubyLoop:'

  #     open(path [, mode [, perm]] )                => io or nil
  #     open(path [, mode [, perm]] ) {|io| block }  => obj
  #
  #  Creates an <code>IO</code> object connected to the given stream,
  #  file, or subprocess.
  #
  #  If <i>path</i> does not start with a pipe character
  #  (``<code>|</code>''), treat it as the name of a file to open using
  #  the specified mode (defaulting to ``<code>r</code>''). (See the table
  #  of valid modes on page 331.) If a file is being created, its initial
  #  permissions may be set using the integer third parameter.
  #
  #  If a block is specified, it will be invoked with the
  #  <code>File</code> object as a parameter, and the file will be
  #  automatically closed when the block terminates. The call
  #  returns the value of the block.
  #
  #  If <i>path</i> starts with a pipe character, a subprocess is
  #  created, connected to the caller by a pair of pipes. The returned
  #  <code>IO</code> object may be used to write to the standard input
  #  and read from the standard output of this subprocess. If the command
  #  following the ``<code>|</code>'' is a single minus sign, Ruby forks,
  #  and this subprocess is connected to the parent. In the subprocess,
  #  the <code>open</code> call returns <code>nil</code>. If the command
  #  is not ``<code>-</code>'', the subprocess runs the command. If a
  #  block is associated with an <code>open("|-")</code> call, that block
  #  will be run twice---once in the parent and once in the child. The
  #  block parameter will be an <code>IO</code> object in the parent and
  #  <code>nil</code> in the child. The parent's <code>IO</code> object
  #  will be connected to the child's <code>$stdin</code> and
  #  <code>$stdout</code>. The subprocess will be terminated at the end
  #  of the block.
  #
  #     open("testfile") do |f|
  #       print f.gets
  #     end
  #
  #  <em>produces:</em>
  #
  #     This is line one
  #
  #  Open a subprocess and read its output:
  #
  #     cmd = open("|date")
  #     print cmd.gets
  #     cmd.close
  #
  #  <em>produces:</em>
  #
  #     Wed Apr  9 08:56:31 CDT 2003
  #
  #  Open a subprocess running the same Ruby program:
  #
  #     f = open("|-", "w+")
  #     if f == nil
  #       puts "in Child"
  #       exit
  #     else
  #       puts "Got: #{f.gets}"
  #     end
  #
  #  <em>produces:</em>
  #
  #     Got: in Child
  #
  #  Open a subprocess using a block to receive the I/O object:
  #
  #     open("|-") do |f|
  #       if f == nil
  #         puts "in Child"
  #       else
  #         puts "Got: #{f.gets}"
  #       end
  #     end
  #
  #  <em>produces:</em>
  #
  #     Got: in Child
  def open(name, *rest, &block)
    path = Type.coerce_to(name, String, :to_str)

    if path._isString and path[0].equal?(?|)
      return IO.popen(path[1..-1], *rest, &block)
    end
    File.open(path, *rest, &block)
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
    f = STDOUT
    if f.equal?(nil)
      raise "STDOUT is nil in Kernel.puts!"
    else
      f.puts(*args)
    end
    nil
  end

  def putc(arg)
    STDOUT.putc(arg)
    arg
  end

  # def rand #  implemented in Kernel2.rb

  def raise(ex_class, message)
    ex = ex_class.exception(message)
    ex._signal
  end

  def raise(ex_class, message, stack)
    ex = ex_class.exception(message)
    ex.set_backtrace(stack)
    ex._signal
  end

  def raise(msg)
    if msg._isString
      raise(RuntimeError, msg)
    else
      # msg should be a subclass of Exception or
      #  an object that returns a new exception
      ex = msg.exception
      if (ex._handler_active)
        ex._reraise
      else
        ex._signal
      end
    end
  end

  def _reraise(ex)
    # _reraise also invoked from IR generated for RubyVCallRaiseNode
    ex._reraise
  end

  def raise
    #  if $! is valid in the caller, the parser will
    #   translate raise to  _reraise
    RuntimeError._signal
  end
  alias fail raise

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

  primitive '`',   '_system:'

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
    resultStr = _system(cmd)
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

  # def throw(aSymbol); end  # implemented in smalltalk
  primitive_nobridge 'throw' , 'throw:'

  # def throw(aSymbol, aValue); end  # implemented in smalltalk
  primitive_nobridge 'throw' , 'throw:with:'

  def debugger
    # signals an Exception which is not trappable by Ruby or Smalltalk
    # and thus returns control to topaz debugger or other GCI main program.
    nil.pause
  end

  def require(name)
    RUBY.require(Type.coerce_to(name, String, :to_str))
  end
end
