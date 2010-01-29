module Kernel

  primitive_nobridge '_resolve_smalltalk_global', 'resolveSmalltalkGlobal:'

  # _smalltalk_global_put for use by bootstrap code only
  primitive_nobridge '__smalltalk_global_put', 'smalltalkUserGlobalsAt:put:'

  # Print messages for stubbed methods
  # presence of classvars disturbs specs, constants disturbs vmunit tests
  # MAGLEV_WARNSTUB = false
  # MAGLEV_SEEN = { }  # TODO, should be a transient hash

  def __stub_warn(msg)
    #if MAGLEV_WARNSTUB
    #  unless MAGLEV_SEEN[msg]
    #    puts "== WARN: STUB: MNI: #{msg}"
    #    MAGLEV_SEEN[msg] = 1
    #  end
    #end
  end

  def load(name, wrap=false)
    if wrap
      raise ArgumentError , 'Kernel.load  , wrap==true not supported yet' # TODO
    end
    RUBY.load(Type.coerce_to(name, String, :to_str))
    true
  end

  def abort(string)
    puts string
    exit(1)
  end

  primitive_nobridge '__at_exit', 'atExit:'

  def at_exit(&block)
    proc = Proc.new(&block)
    __at_exit(proc.__block )
    proc
  end

  # See ruby-core:20222 for a discussion on whether to use Kernel#require
  # or some other private implementation for autoload.
  primitive_nobridge 'autoload', 'rubyKernelAutoload:file:'
  primitive_nobridge 'autoload?', 'rubyKernelAutoloadFileFor:'

  # binding defined in Kernel2.rb

  primitive_nobridge '__last_dnu_protection', '_lastDnuProtection'

  def method_missing(method_id, *args)
    prot = __last_dnu_protection()
    type = if (prot._equal?(0))
             'undefined method'
           elsif (prot._equal?(1))
             'protected method'
           else
             'private method'
           end

    exc = NoMethodError.exception("NoMethodError: #{type} `#{method_id}' for #{self}")
    exc.__init(method_id , args, 1)  # FOR NOW, ASSUME envId 1
    exc.__signal
  end

  def caller(skip=0, limit=1000)
    # returns an Array of Strings, each element describes a stack frame
    unless skip._isFixnum
      raise ArgumentError
    end
    res = Thread.__backtrace(false, limit)
    if (skip > 0)
      res = res[skip, res.length]
    end
    res
  end

  # def catch(aSymbol, &aBlock); end
  primitive_nobridge_env 'catch&' , 'catch', ':do:'

  def debugger
    # signals an Exception which is not trappable by Ruby or Smalltalk
    # and thus returns control to topaz debugger or other GCI main program.
    nil.pause
  end

  primitive_nobridge '__eval_with_position', '_eval:binding:with:fileName:lineNumber:lexPath:'

  def eval(__lex_path, str, binding, file, *args)
    # __lex_path arg is synthesized by the parser in calling code
    line = 0
    if (asz = args.size) > 0 
      if (asz > 1)
        raise ArgumentError, 'too many args'
      end
      line = args[0]
    end
    if binding._equal?(nil)
      ctx = self.__binding_ctx(0)
      bnd = Binding.new(ctx, self, nil)
    else
      bnd = binding
      unless bnd._is_a?(Binding) ; raise TypeError,'not a Binding' ; end
    end
    vcgl = [ self.__getRubyVcGlobal(0x20) ,
      self.__getRubyVcGlobal(0x21) , nil ]
    blk = bnd.block
    unless blk._equal?(nil)
      vcgl << blk
    end
    res = __eval_with_position(str, bnd, vcgl, file, line, __lex_path )
    vcgl[0].__storeRubyVcGlobal(0x20)
    vcgl[1].__storeRubyVcGlobal(0x21)
    res
  end

  def eval(__lex_path, str, file)
    # no bridge methods for this and subsequent variants
    # __lex_path arg is synthesized by the parser in calling code
    ctx = self.__binding_ctx(0)
    bnd = Binding.new(ctx, self, nil)
    vcgl = [ self.__getRubyVcGlobal(0x20) ,
             self.__getRubyVcGlobal(0x21) , self ]
    blk = bnd.block
    unless blk._equal?(nil)
      vcgl << blk
    end
    res = __eval_with_position(str, bnd, vcgl, file, 0 , __lex_path)
    vcgl[0].__storeRubyVcGlobal(0x20)
    vcgl[1].__storeRubyVcGlobal(0x21)
    res
  end

  def eval(__lex_path, str)
    # no bridge methods for this and subsequent variants
    # __lex_path arg is synthesized by the parser in calling code
    ctx = self.__binding_ctx(0)
    bnd = Binding.new(ctx, self, nil)
    vcgl = [ self.__getRubyVcGlobal(0x20) ,
             self.__getRubyVcGlobal(0x21) , self ]
    blk = bnd.block
    unless blk._equal?(nil)
      vcgl << blk
    end
    res = __eval_with_position(str, bnd, vcgl, nil, 0 , __lex_path)
    vcgl[0].__storeRubyVcGlobal(0x20)
    vcgl[1].__storeRubyVcGlobal(0x21)
    res
  end

  def eval(__lex_path, str, &blk)
    # __lex_path arg is synthesized by the parser in calling code
    ctx = self.__binding_ctx(0)
    bnd = Binding.new(ctx, self, nil)
    vcgl = [ self.__getRubyVcGlobal(0x20) ,
             self.__getRubyVcGlobal(0x21) , self ]
    unless blk._equal?(nil)
      vcgl << blk
    end
    res = __eval_with_position(str, bnd, vcgl, nil, 0, __lex_path )
    vcgl[0].__storeRubyVcGlobal(0x20)
    vcgl[1].__storeRubyVcGlobal(0x21)
    res
  end

  # def eval(str) ; end 
  # could be sent via  __send__ or send, but not supported yet 
  # You must code evals explicitly.

  def eval(__lex_path, str, binding)
    # __lex_path arg is synthesized by the parser in calling code
    if binding._equal?(nil)
      ctx = self.__binding_ctx(0)
      bnd = Binding.new(ctx, self, nil)
    else
      bnd = binding
      unless bnd._is_a?(Binding) ; raise TypeError,'not a Binding' ; end
    end
    vcgl = [ self.__getRubyVcGlobal(0x20) ,
      self.__getRubyVcGlobal(0x21), nil ]
    blk = bnd.block
    unless blk._equal?(nil)
      vcgl << blk
    end
    res = __eval_with_position(str, bnd, vcgl, nil, 0, __lex_path )
    vcgl[0].__storeRubyVcGlobal(0x20)
    vcgl[1].__storeRubyVcGlobal(0x21)
    res
  end

  def exit(arg=0)
    status = 9
    if (arg._equal?(true))
      status = 0
    elsif (arg._isInteger)
      status = arg
    end
    raise SystemExit.new(status)
  end

  primitive 'format*', 'sprintf:with:'

  def gets(sep=$/)
    # TODO: Need to use-up ARGV first...
    STDIN.gets(sep)
  end

  primitive 'global_variables', 'rubyGlobalVariables'

  def gsub(regex, string)
    string = Type.coerce_to(string, String, :to_str)
    str = self.__getRubyVcGlobal(0x21) # get callers $_
    if str._equal?(nil)
      raise TypeError, 'Kernel.gsub, caller frame has no reference to $_ '
    end
    unless str._isString
      raise TypeError, '$_ is not a String'
    end
    res = str.gsub(regex, string)
    res.__storeRubyVcGlobal(0x21) # store into caller's $_
    res
  end

  def gsub(regex, &block)
    # $~ and related variables will be valid in block if
    #   blocks's home method and caller's home method are the same
    str = self.__getRubyVcGlobal(0x21) # get callers $_
    if str._equal?(nil)
      raise TypeError, 'Kernel.gsub, caller frame has no reference to $_ '
    end
    unless str._isString
      raise TypeError, '$_ is not a String'
    end
    start = 0
    out = ''
    str.__get_pattern(regex, true).__each_match_vcgl(str, 0x30) do |match|
      out << str._gsub_copyfrom_to(start, match.begin(0))
      saveTilde = block.__fetchRubyVcGlobal(0);
      begin
        block.__setRubyVcGlobal(0, match);
        out << block.call(match[0]).to_s
      ensure
        block.__setRubyVcGlobal(0, saveTilde);
      end
      start = match.end(0)
    end
    out << str.__copyfrom_to(start + 1, str.length)
    out.__storeRubyVcGlobal(0x21) # store into caller's $_
    out
  end

  def gsub!(regex, string)
    str = self.__getRubyVcGlobal(0x21) # get callers $_
    if str._equal?(nil)
      raise TypeError, 'Kernel.gsub! , caller frame has no reference to $_ '
    end
    unless str._isString
      raise TypeError, '$_ is not a String'
    end
    str.gsub!(regex, string)
  end

  def gsub!(regex, &block)
    str = self.__getRubyVcGlobal(0x21) # get callers $_
    if str._equal?(nil)
      raise TypeError, 'Kernel.gsub! , caller frame has no reference to $_ '
    end
    unless str._isString
      raise TypeError, '$_ is not a String'
    end
    start = 0
    out = str.class.new
    str.__get_pattern(regex, true).__each_match_vcgl(str, 0x30) do |match|
      out << str._gsub_copyfrom_to(start, match.begin(0) )
      saveTilde = block.__fetchRubyVcGlobal(0);
      begin
        block.__setRubyVcGlobal(0, match);
        out << block.call(match[0]).to_s
      ensure
        block.__setRubyVcGlobal(0, saveTilde);
      end
      start = match.end(0)
    end
    out << str.__copyfrom_to(start + 1, str.length)
    if str == out
      res = nil
    else
      str.replace(out)  # replace detects frozen
      res = str
    end
    res
  end

  # This implementation of include handles include from a main program
  primitive_nobridge '__include_module',  'includeRubyModule:'
  def include(*names)
    # this variant gets bridge methods
    names.reverse.each do |name|
      __include_module(name)
    end
  end
  def include(name)
    # variant needed for bootstrap
    __include_module(name)
  end

  # def loop(&block) ; end
  primitive_env 'loop&', '_rubyLoop', ':'

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
#  Following pipe behavior probably not implemented yet in Maglev. 
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

    if path._isString and path[0]._equal?(?|)
      return IO.popen(path[1..-1], *rest, &block)
    end
    File.open(path, *rest, &block)
  end

  def p(*args)
    n = 0
    lim = args.length
    f = $stdout
    while n < lim
      f.write(args[n].inspect)
      f.write("\n")   # TODO observe record sep global
      n += 1
    end
    nil
  end

  def print(*args)
    $stdout.print(*args)
    nil
  end

  def printf(a, b, c, *d)
    if (a._kind_of?(IO))
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
      $stdout.printf(a, *args)
    end
  end

  def printf(a, b, c)
    if (a._kind_of?(IO))
      a.printf(b, c)
    else
      $stdout.printf(a, b, c)
    end
  end

  def printf(a, b)
    if (a._kind_of?(IO))
      a.printf(b)
    else
      $stdout.printf(a, b)
    end
  end

  def printf(a)
    $stdout.printf(a)
  end

  # def proc ...  in Kernel2.rb

  def puts(*args)
    f = $stdout
    if f._equal?(nil)
      raise "$stdout is nil in Kernel.puts!"
    else
      f.puts(*args)
    end
    nil
  end

  def putc(arg)
    $stdout.putc(arg)
    arg
  end

  def raise(ex_class, message)
    if ex_class._isString
      msg = ex_class 
      if message._isString
         msg << message
      end
      raise(RuntimeError, msg)
    else
      ex = ex_class.exception(message)
      ex.__signal
    end
  end

  def raise(ex_class, message, stack)
    if ex_class._isString
      msg = ex_class 
      if message._isString
         msg << message
      end
      raise(RuntimeError, msg, stack)
    else
      ex = ex_class.exception(message)
      ex.set_backtrace(stack)
      ex.__signal
    end
  end

  def raise(msg)
    if msg._isString
      raise(RuntimeError, msg)
    else
      # msg should be a subclass of Exception or
      #  an object that returns a new exception
      ex = msg.exception
      if (ex.__handler_active)
        ex.__reraise
      else
        ex.__signal
      end
    end
  end

  def __reraise(ex)
    # _reraise also invoked from IR generated for RubyVCallRaiseNode
    ex.__reraise
  end

  def raise
    #  if $! is valid in the caller, the parser will
    #   translate raise to  __reraise
    RuntimeError.__signal
  end
  alias fail raise

  # def rand #  implemented in Kernel2.rb

  def readline(sep=$/)
    res = self.gets(sep)
    if res._equal?(nil)
      raise EOFError
    end
    res
  end

  def readlines(sep=$/)
    res = []
    line = self.gets(sep)
    while line._not_equal?(nil)
      res << line
      line = self.gets(sep)
    end
    res
  end
 
  def require(name)
    RUBY.require(Type.coerce_to(name, String, :to_str))
  end

  def scan(pattern)
    str = self.__getRubyVcGlobal(0x21) # get callers $_
    if str._equal?(nil)
      raise TypeError, 'Kernel.scan, caller frame has no reference to $_ '
    end
    unless str._isString
      raise TypeError, '$_ is not a String'
    end
    str._scan(pattern)
  end

  def scan(pattern, &block)
    str = self.__getRubyVcGlobal(0x21) # get callers $_
    if str._equal?(nil)
      raise TypeError, 'Kernel.scan, caller frame has no reference to $_ '
    end
    unless str._isString
      raise TypeError, '$_ is not a String'
    end
    str._scan(pattern, &block)
  end

  primitive_nobridge '__select*', 'selectRead:write:error:timeout:'
  
  def select(reads, writes=nil, errs=nil, timeout=nil)
    if timeout._isFixnum
      ms = timeout * 1000 
      unless ms._isFixnum && ms >= 0
        raise ArgumentError , "IO#select, timeout not representable as Fixnum milliseconds >=0"
      end
    elsif timeout._not_equal?(nil)
      timeout = Type.coerce_to(timeout, Float, :to_f) 
      ms = (timeout * 1000.0 ).to_int  
      unless ms._isFixnum && ms >= 0
        raise ArgumentError , "IO#select, timeout not representable as Fixnum milliseconds >=0"
      end
    end
    Kernel.__select(reads, writes, errs, *[ ms ])
  end

  def split(pattern=nil, limit=Undefined)
    str = self.__getRubyVcGlobal(0x21) # get callers $_
    if str._equal?(nil)
      raise TypeError, 'Kernel.split, caller frame has no reference to $_ '
    end
    unless str._isString
      raise TypeError, '$_ is not a String'
    end
    str.split(pattern, limit)
  end

  # sleep behavior
  # PickAxe book   says argument of zero means infinite sleep
  # MRI behavior,  sleep(0) returns immediately
  # Maglev, sleep(0) will be equivalent to Thread.pass

  primitive_nobridge '__sleep_ms', '_highPriorityWaitForMilliseconds:'

  def sleep(numeric=0)
    # returns number of seconds slept
    if numeric._isInteger
      ms = numeric * 1000
    elsif
      ms = (1000.0 * numeric).to_i
    end
    ms_slept = __sleep_ms(ms)
    ms_slept.__divide(1000)
  end

  def sleep_ms(milliseconds=0)
    # Gemstone addition
    # returns number of milliseconds slept
    ms = milliseconds
    unless milliseconds._isInteger
      ms = milliseconds.to_i
    end
    __sleep_ms(ms)
  end

  def split(pattern=nil, limit=nil)
    $_ .split(pattern, limit)
  end

  primitive 'sprintf*', 'sprintf:with:'

  primitive_nobridge '__system_exec', '_system:'

  def `(arg)
    # called from generated code
    arg = Type.coerce_to(arg, String, :to_str)
    arr = __system_exec(arg)  #   raw_status is arr[0]
    status = arr[1]
    unless status._equal?(0)
      Errno.raise_errno(status, arg)
    end
    arr[2]
  end

  def sub(pattern, replacement)
    str = self.__getRubyVcGlobal(0x21) # get callers $_
    if str._equal?(nil)
      raise TypeError, 'Kernel.sub , caller frame has no reference to $_ '
    end
    unless str._isString
      raise TypeError, '$_ is not a String'
    end
    replacement = Type.coerce_to(replacement, String, :to_str)
    regex = str.__get_pattern(pattern, true)
    r = if (match = regex.__match_vcglobals(str, 0x30))
          str.__replace_match_with(match, replacement)
        else
          str.dup
        end
    r.__storeRubyVcGlobal(0x21) # store into caller's $_
    r
  end

  def sub(pattern, &block)
    str = self.__getRubyVcGlobal(0x21) # get callers $_
    if str._equal?(nil)
      raise TypeError, 'Kernel.sub , caller frame has no reference to $_ '
    end
    unless str._isString
      raise TypeError, '$_ is not a String'
    end
    regex = str.__get_pattern(pattern, true)
    if (match = regex.__match_vcglobals(str, 0x30))
       res = str.__replace_match_with(match, block.call(match[0]).to_s)
    else
       res = str.dup
    end
    res.__storeRubyVcGlobal(0x21) # store into caller's $_
    res
  end

  def sub!(pattern, replacement)
    str = self.__getRubyVcGlobal(0x21) # get callers $_
    if str._equal?(nil)
      raise TypeError, 'Kernel.sub! , caller frame has no reference to $_ '
    end
    unless str._isString
      raise TypeError, '$_ is not a String'
    end
    regex = str._get_pattern(pattern, true)
    # stores into caller's $~
    if match = regex.__match_vcglobals(str, 0x30)
      str.replace( str.__replace_match_with(match, replacement))
      str
    else
      nil
    end
  end

  def sub!(pattern, &block)
    str = self.__getRubyVcGlobal(0x21) # get callers $_
    if str._equal?(nil)
      raise TypeError, 'Kernel.sub! , caller frame has no reference to $_ '
    end
    unless str._isString
      raise TypeError, '$_ is not a String'
    end
    regex = str.__get_pattern(pattern, true)
    if match = regex.__match_vcglobals(str, 0x30)
      replacement = block.call(match[0])
      str.replace( str.__replace_match_with(match, replacement))
      str
    else
      nil
    end
  end

  def __system(arg)
    # called from generated code
    arg = Type.coerce_to(arg, String, :to_str)
    arr = __system_exec(arg)  #   raw_status is arr[0]
    status = arr[1]
    unless status._equal?(0)
      Errno.raise_errno(status, arg)
    end
    arr[2]
  end

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
    arr = __system_exec(cmd)  #   raw_status is arr[0]
    status = arr[1]
    if status._equal?(0)
      # print result string per MRI behavior, not documented in Pickaxe book
      puts arr[2]
      return true
    end
    return false
  end

  def trap(signal, proc)
    __stub_warn("Kernel#trap(signal, proc)")
  end

  def trap(signal, &block)
    __stub_warn("Kernel#trap(signal, &block)")
  end

  def __as_file(arg)
    if arg._is_a?(File)
      f = arg
    else
      fn = Type.coerce_to(arg, String, :to_str)
      begin
        f = File.open(fn)
      rescue
        f = nil
      end
    end
    f
  end

  def __close_file(f)
    if f._is_a?(File)
      f.close
    end
  end

  def test(cmd, file)
    f = nil
    res = false
    begin
      f = self.__as_file(file)
      if cmd._equal?( ?A )
        if f._equal?(nil)
          File.open(file) # raises ENOENT
        end
        res = f.atime
      elsif cmd._equal?( ?C )
        if f._equal?(nil)
          File.open(file) # raises ENOENT
        end
        res = f.ctime
      elsif cmd._equal?( ?M )
        if f._equal?(nil)
          File.open(file) # raises ENOENT
        end
        res = f.mtime
      elsif cmd._equal?( ?b )
        res = f._not_equal?(nil) && f.lstat.blockdev?
      elsif cmd._equal?( ?c )
        res = f._not_equal?(nil) && f.lstat.chardev?
      elsif cmd._equal?( ?d )
        res = f._not_equal?(nil) && f.lstat.directory?
      elsif cmd._equal?( ?e )
        res = f._not_equal?(nil)
      elsif cmd._equal?( ?f )
        res = f._not_equal?(nil) && f.lstat.file?
      elsif cmd._equal?( ?g )
        res = f._not_equal?(nil) && f.lstat.setgid?
      elsif cmd._equal?( ?k )
        res = f._not_equal?(nil) && f.lstat.sticky?
      elsif cmd._equal?( ?l )
        res = f._not_equal?(nil) && f.lstat.symlink?
      elsif cmd._equal?( ?p )
        res = f._not_equal?(nil) && f.lstat.pipe?  # a fifo
      elsif cmd._equal?( ?S )
        res = f._not_equal?(nil) && f.lstat.socket?
      elsif cmd._equal?( ?u )
        res = f._not_equal?(nil) && f.lstat.setuid?
      elsif cmd._equal?( ?s )
        res = nil
        if f._not_equal?(nil) && (sz = f.lstat.size)
          res = sz
        end
      elsif cmd._equal?( ?z )
        res = f._not_equal?(nil) && f.lstat.size == 0
      elsif cmd._equal?( ?r )
        res = f._not_equal?(nil) && f.lstat.readable?
      elsif cmd._equal?( ?R )
        res = f._not_equal?(nil) && f.lstat.readable_real?
      elsif cmd._equal?( ?o )
        res = f._not_equal?(nil) && f.lstat.owned?
      elsif cmd._equal?( ?O )
        res = f._not_equal?(nil) && f.lstat.rowned?
      elsif cmd._equal?( ?G )
        res = f._not_equal?(nil) && f.lstat.rgrpowned?
      elsif cmd._equal?( ?w )
        res = f._not_equal?(nil) && f.lstat.writable?
      elsif cmd._equal?( ?W )
        res = f._not_equal?(nil) && f.lstat.writable_real?
      elsif cmd._equal?( ?x )
        res = f._not_equal?(nil) && f.lstat.executable?
      elsif cmd._equal?( ?W )
        res = f._not_equal?(nil) && f.lstat.executable_real?
      else
        raise ArgumentError , 'Kernel#test , invalid first arg'
      end
    ensure
      self.__close_file(f)
    end
    res
  end

  def test(cmd, file1, file2)
    res = false
    fa = nil
    fb = nil
    begin
      fa = self.__as_file(file1)
      fb = self.__as_file(file2)
      mta = fa._equal?(nil) ? -1 : fa.mtime
      mtb = fa._equal?(nil) ? -1 : fa.mtime
      if cmd._equal?( ?= )
        # return true if modification times equal
        res =  mta >= 0 && mtb >= 0 &&  mta == mtb
      elsif cmd._equal?( ?< )
        # return true if file1.mtime < file2.mtime
        res =  mta >= 0 && mtb >= 0 &&  mta < mtb
      elsif cmd._equal?( ?> )
        # return true if file1.mtime > file2.mtime
        res =  mta >= 0 && mtb >= 0 &&  mta > mtb
      elsif cmd._equal?( ?- )
        # return true if file1 is a hard link to file2
        raise NotImplementedError , 'Kernel#test ?- '
      else
        raise ArgumentError
      end
    ensure
      self.__close_file(fa)
      self.__close_file(fb)
    end
    res
  end


  # def throw(aSymbol); end  # implemented in smalltalk
  primitive_nobridge 'throw' , 'throw:'

  # def throw(aSymbol, aValue); end  # implemented in smalltalk
  primitive_nobridge 'throw' , 'throw:with:'

  primitive_nobridge '__trace_global_assign&', 'traceGlobalVarAssign:block:'

  def __call_tracevar_block(new_value, &blk)
    # called from Smalltalk
    blk.call(new_value)
  end
 
  def __trace_var(name, &blk)
    unless name._isSymbol
      name = Type.coerce_to(name, String, :to_str)
      name = name.to_sym
    end
    __trace_global_assign(name, &blk)
  end

  def trace_var(name, cmd='ignored', &blk)
    # cmd is currently ignored
    # name must be a String or Symbol beginning with '$' 
    # if blk is nil, has no effect
    if block_given?
      __trace_var(name, &blk) 
    end
  end

  def untrace_var(name, cmd='ignored')
    # cmd is currently ignored
    # name must be a String or Symbol beginning with '$' 
    blk = nil  #  shut off tracing for specified name
    __trace_var(name, &blk) 
  end

end
