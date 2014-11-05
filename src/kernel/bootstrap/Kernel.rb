module Kernel

  # This is for bootstrap code only
  primitive_nobridge '__resolve_smalltalk_global', 'resolveSmalltalkGlobal:'

  # _smalltalk_global_put for use by bootstrap code only
  primitive_nobridge '__smalltalk_global_put', 'smalltalkUserGlobalsAt:put:'

  # Use this method to expose a Smalltalk class to Ruby land under a
  # specific name. It will install the class within the current
  # namespace under the specified name
  primitive 'expose_smalltalk_global_as', 'exposeSmalltalkGlobal:as:'

  # Loads and executes the Ruby program in the file +name+.  If the
  # filename does not resolve to an absolute path, the file is searched for
  # in the library directories listed in <tt>$:</tt>.  If the optional
  # +wrap+ parameter is +true+, the loaded script will be executed under an
  # anonymous module, protecting the calling prgram's global namespace.  In
  # no circumstances will any local variables in the loaded file be
  # propagated to the loading environment.
  def load(name, wrap=false)
    if wrap
      raise ArgumentError , 'Kernel.load  , wrap==true not supported yet' # TODO
    end
    RUBY.load(Maglev::Type.coerce_to(name, String, :to_str))
    true
  end

  # Terminates execution immediately with an exit code of 1.  The optional
  # String parameter is written to standard error before the program
  # terminates.
  def abort(string=nil)
    unless string._equal?(nil)
      unless string._isString
        raise TypeError , 'arg to Kernel#abort must be a String'
      end
      $stderr.write("#{string}\n")
    end
    exit(1)
  end

  primitive_nobridge '__at_exit', 'atExit:'

  # Converts block toa Proc object (and therefore binds it at the point of
  # call) and registers it for execution when the program exits.  If
  # multiple hanlders are registered, they are executed in reverse order of
  # registration.
  def at_exit(&block)
    proc = Proc.new(&block)
    __at_exit(proc.__block )
    proc
  end

  #--
  # See ruby-core:20222 for a discussion on whether to use Kernel#require
  # or some other private implementation for autoload.
  #++
  # call-seq:
  #   autoload(name, file_name) => nil
  #
  # Registers +file_name+ to be loaded (using <tt>Kernel.require</tt>) the
  # first time that the module +name+ (which may be a string or a symbol)
  # is accessed.
  primitive_nobridge 'autoload', 'rubyKernelAutoload:file:'

  # call-seq:
  #   autoload?(name) => file_name or nil
  #
  # Returns the name of the file that will be autoloaded when the string or
  # symbol +name+ is referenced in the top-level context or returns +nil+
  # if there is no associated autoload.
  primitive_nobridge 'autoload?', 'rubyKernelAutoloadFileFor:'

  # binding defined in Kernel2.rb

  primitive_nobridge '__last_dnu_protection', '_lastDnuProtection'

  # Returns the name of the current method or +nil+ outside the context of
  # a method.
  def __method__(*args, &block)  # added for 1.8.7
    # always come here via a bridge method
    unless args.__size._equal?(0)
      raise ArgumentError, 'too many args'
    end
    ctx_arr = self.__binding_ctx(1)
    mth = ctx_arr[-1]
    sel = nil
    unless mth._equal?(nil)
      sel = mth.__name
    end
    sel
  end

  # def __callee ; end # not needed for 1.9, generated code uses __method__

  def method_missing(method_id, *args)
    prot = __last_dnu_protection()
    msg = if (prot._equal?(0))
            "NoMethodError: undefined method `"
          elsif (prot._equal?(1))
            "NoMethodError: protected method `"
          else
            "NoMethodError: private method `"
          end
    msg.__append(method_id)
    msg.__append( :"' for " )
    msg.__append( self.__name_for_mnu )  # Fix trac 752, don't use to_s
    exc = NoMethodError.exception(msg)
    exc.__init(method_id , args, 1)  # FOR NOW, ASSUME envId 1
    exc.__signal
  end

  # Returns the current execution stack -- an array containing strings in
  # the form <tt>file:line</tt>, or <tt>file:line: in 'method'</tt>.  The
  # optional +skip+ parameter determines the number of initial stack
  # entries to omit from the result.  The optional +limit+ parameter
  # determines the maximum number of frames to return.
  def caller(skip=1, limit=1000)
    # returns an Array of Strings, each element describes a stack frame
    unless skip._isFixnum
      raise ArgumentError
    end
    res = Thread.__backtrace(false, limit)
    idx = 1  # skip frame of Kernel.caller
    if (skip > 0)
      idx += skip
    end
    res[idx, res.length]
  end

  # call-seq:
  #   catch(object=Object.new) { block } => obj
  #
  # +catch+ executes its block.  If a +throw+ is encountered, Ruby searches
  # up its stack fora a +catch+ block with a parameter identical to the
  # +throw+'s parameter.  If found, that block is terminated, and +catch+
  # returns the value given as the second parameter to +throw+.  If +throw+
  # is not called, the block terminates normally.
  primitive_nobridge_env 'catch&' , 'catch', ':do:'

  # MagLev only.  Hook method used in Ruby Spec Framework to invoke a
  # debugger.  The MagLev implementation simply pauses.  If the VM was
  # started in debug mode (<tt>-d</tt> on the command line), then the VM
  # will drop into the Topaz debugger.
  def debugger
    # signals an Exception which is not trappable by Ruby or Smalltalk
    # and thus returns control to topaz debugger or other GCI main program.
    nil.pause
  end

  def __fd_for_exec(arg)
    if arg._isFixnum
      return arg  if arg >= 0 && arg <= 2
    elsif arg._isSymbol
      return 0 if arg._equal?(:in)
      return 1 if arg._equal?(:out)
      return 2 if arg._equal?(:err)
    elsif arg._is_a?(File)
      return 0 if arg._equal?(STDIN)
      return 1 if arg._equal?(STDOUT)
      return 2 if arg._equal?(STDERR)
    end
    return nil
  end

  def __exec_prepare_fd_strm(fd_desc, v, dups_arr)
    if (strm = __fd_for_exec(v))
      dups_arr << fd_desc
      dups_arr << strm
      return true
    elsif v._isString || v._isArray
      if v._isString
        f = File.open(v, (fd_desc == 0) ? 'r' : 'a' )
      else
        f = File.open(*v)
      end
      f.__disable_autoclose
      dups_arr << fd_desc
      dups_arr << f.__fileno
      return true
    else
      return false
    end
  end

  primitive '__waitpid2' , 'waitpid:flags:'

  # call-seq:
  #   exec(command <, args*>)
  #   exec(<env, > command <, args*>, <options>)
  # Replaces the current process by running the given external command.
  # The following keys in options are not implemented
  #    :pgroup, :rlimit_xxx , :umask .
  def exec(*args)
    arr = __pre_exec(*args)
    original_wd = arr[4]
    begin
      cmd  = arr[0]
      cmd = __find_on_path( cmd )
      env_arr = arr[1]
      dups_arr = arr[2]
      arguments = arr[3]
      child_pid = self.__execv(cmd, 0, env_arr, dups_arr, *arguments)
      if child_pid < 0
        Errno.handle( - child_pid )
      end
      raise 'Unexpected return from Kernel.__execv'
    ensure
      Errno.handle(Dir.__chdir(original_wd), "chdir back to #{original_wd}")
    end
  end

  def spawn(*args)
    arr = __pre_exec(*args)
    original_wd = arr[4]
    child_pid = nil
    begin
      cmd  = "sh"
      env_arr = arr[1]
      dups_arr = arr[2]
      arguments = ["-c"]
      command = [arr[0]]
      command.concat(arr[3])
      arguments << command.join(" ")
      child_pid = self.__execv(cmd, 1, env_arr, dups_arr, *arguments)
      if child_pid < 0
        Errno.handle( - child_pid )
      end
    ensure
      Errno.handle(Dir.__chdir(original_wd), "chdir back to #{original_wd}")
    end
    child_pid
  end

  def system(*args)
    child_pid = self.spawn(*args)
    done = false
    count = 0
    while !done
      count += 1
      arr = Process.waitpid2(child_pid, 0)
      done = arr[0] != 0
    end
    $?.success?
  end

  def __pre_exec(*args)
    # result is [ cmd, env_arr, dups_arr, arguments , original_wd]
    firstarg = args[0]
    env = nil
    env_arr = [ false , true ]  # env_arr[0] is unsetenv_others arg to prim
    # env_arr[1] is close_others arg to prim
    options = nil
    if firstarg._isHash
      env = firstarg
      env.each_pair { |k,v|
        if k._isString
          vstr = nil
          unless v._equal?(nil)
            vstr = Maglev::Type.coerce_to(v, String, :to_str)
          end
          env_arr << k
          env_arr << vstr
        else
          raise ArgumentError, "non String key #{k} in env arg to exec"
        end
      }
      cmd = args[1]
      idx = 2
    else
      cmd = firstarg
      idx = 1
    end
    lastarg = args[-1]
    lim = args.length
    dups_arr = []
    original_wd = Dir.getwd
    wd_for_exec = nil
    begin
      if lastarg._isHash
        options = lastarg
        options.each_pair { |k, v|
          if k._isSymbol
            if k._equal?(:chdir)
              wd_for_exec = v
            elsif k._equal?(:close_others)
              if v._equal?(true)
                env_arr[1] = true
              elsif v._equal?(false)
                env_arr[1] = false
              else
                raise ArgumentError, "value #{v} for :close_others neither true nor false, in exec"
              end
            elsif k._equal?( :unsetenv_other )
              if v._equal?(true)
                env_arr[0] = true
              else
                raise ArgumentError, "value #{v} for :unsetenv_other is not true, in exec"
              end
            else
              raise ArgumentError, "option #{k} not recognized for exec"
            end
          elsif k._is_a?(IO)
            unless v._equal?(:close)
              raise ArgumentError, "anIO=>#{v} not a valid option for exec"
            end
          elsif (fd_desc = __fd_for_exec(k))
            unless __exec_prepare_fd_strm(fd_desc, v, dups_arr)
              raise ArgumentError, "bad stream #{v} for #{k}=>stream in exec"
            end
          elsif k._isArray
            k.each { | kelem |
              if (k_desc = __fd_for_exec(kelem))
                unless __exec_prepare_fd_strm(k_desc, v, dups_arr)
                  raise ArgumentError, "bad stream #{v} for #{kelem} of #{k}=>stream in exec"
                end
              else
                raise ArgumentError, "invalid element #{kelem} of #{k}=>stream in exec"
              end
            }
          else
            raise ArgumentError, "invalid option #{k} for exec"
          end
        }
        lim -= 1
      end
      cmd = Maglev::Type.coerce_to(cmd, String, :to_str)
      arguments = []
      while idx < lim
        arguments << Maglev::Type.coerce_to(args[idx], String, :to_str)
        idx += 1
      end
      if wd_for_exec
        Dir.chdir(wd_for_exec)
      end
      return [ cmd, env_arr, dups_arr, arguments , original_wd]
    end
  end

  def __find_on_path(cmd)
    if cmd.index("/") == 0 || cmd.index("./") == 0 || cmd.index("../") == 0
      return cmd
    else
      # TODO:
      #  1. ~ expansion not done
      #  2. zero length path components not yet interpreted as "."
      ENV['PATH'].split(':').each do |comp|
        full_cmd = File.join(comp, cmd)
        return full_cmd if File.exist?(full_cmd)
      end
    end
    raise Errno::ENOENT.new("No such file or directory - #{cmd}")
  end

  primitive '__execv*', 'execv:opcode:envVars:fdDups:args:'

  primitive_nobridge '__eval_with_position', '_eval:binding:with:fileName:lineNumber:'

  # call-seq:
  #   eval(string <, binding <, file <, line> > >) => obj
  #
  # Evaluates the Ruby expression(s) in +string+.  If +binding+ is given,
  # the evaluation is performed in its context.  The binding must be a
  # +Binding+ object.  If the optional +file+ and +line+ parameters are
  # present, they will be used when reporting syntax errors.
  def eval(*args, &block_arg)
    #   should always come here via a bridge method , thus 0x3N for vcgl ...
    nargs = args.size
    if nargs < 1
      raise ArgumentError, 'too few args'
    end
    if nargs > 4
      raise ArgumentError, 'too many args'
    end
    lex_path = self.__getRubyVcGlobal(0x32) # __lexPath, synthesized by AST to IR code in .mcz
    str = args[0]
    bnd = args[1]
    file = args[2]
    line = args[3]
    unless file._equal?(nil)
      file = Maglev::Type.coerce_to(file, String, :to_str)
    end
    if line._equal?(nil)
      line = 0
    else
      line = Maglev::Type.coerce_to(line, Fixnum, :to_int)
    end
    if bnd._equal?(nil)
      ctx = self.__binding_ctx(1)
      bnd = Binding.new(ctx, self, block_arg)
      bnd.__set_lex_scope(lex_path)
    else
      unless bnd._is_a?(Binding) ; raise TypeError,'not a Binding' ; end
      # lex_path arg ignored, passed binding has precedence
    end
    vcgl = [ self.__getRubyVcGlobal(0x30) ,
             self.__getRubyVcGlobal(0x31) , nil ]
    bblk = bnd.block
    unless bblk._equal?(nil)
      vcgl << bblk
    end
    res = __eval_with_position(str, bnd, vcgl, file, line )
    vcgl[0].__storeRubyVcGlobal(0x30)
    vcgl[1].__storeRubyVcGlobal(0x31)
    res
  end

  def __cext_eval(*args, &block_arg)
    # called from rb_eval_string_ in C extension implementation
    nargs = args.size
    if nargs < 1
      raise ArgumentError, 'too few args'
    end
    if nargs > 4
      raise ArgumentError, 'too many args'
    end
    str = args[0]
    bnd = args[1]
    file = args[2]
    line = args[3]
    unless file._equal?(nil)
      file = Maglev::Type.coerce_to(file, String, :to_str)
    end
    if line._equal?(nil)
      line = 0
    else
      line = Maglev::Type.coerce_to(line, Fixnum, :to_int)
    end
    # ctx = self.__binding_ctx(1)
    bnd = Binding.__basic_new( nil )
    vcgl = [ self.__getRubyVcGlobal(0x30) ,
             self.__getRubyVcGlobal(0x31) , nil ]
    # bblk = bnd.block
    #unless bblk._equal?(nil)
    #  vcgl << bblk
    #end
    res = __eval_with_position(str, bnd, vcgl, file, line )
    res
  end

  # Initiates the termination of the Ruby script by raising the
  # +SystemExit+ exception. This exception may be caught. The optional
  # +arg+ parameter is used to return a status code to the invoking
  # environment.
  def exit(arg=0)
    status = 9
    if (arg._equal?(true))
      status = 0
    elsif (arg._isInteger)
      status = arg
    end
    raise SystemExit.new(status)
  end

  # Similar to <tt>Kernel.exit</tt>, but exception handling,
  # <tt>at_exit</tt> functions and finalizers are bypassed.
  def exit!(arg=0)
    status = 9
    if (arg._equal?(true))
      status = 0
    elsif (arg._isInteger)
      status = arg
    end
    ex = SystemExit.new(status)
    ex.run_at_exit_handlers = false
    raise ex
  end

  primitive 'format*', 'sprintf:with:'

  def gets(sep=$/)
    $stdin.gets(sep)
  end

  primitive 'global_variables', 'rubyGlobalVariables'

  def gsub(regex, string)
    string = Maglev::Type.coerce_to(string, String, :to_str)
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
      saveTilde = block.__fetchRubyVcGlobal(0)
      begin
        block.__setRubyVcGlobal(0, match)
        out << block.call(match[0]).to_s
      ensure
        block.__setRubyVcGlobal(0, saveTilde)
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
      saveTilde = block.__fetchRubyVcGlobal(0)
      begin
        block.__setRubyVcGlobal(0, match)
        out << block.call(match[0]).to_s
      ensure
        block.__setRubyVcGlobal(0, saveTilde)
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
    path = Maglev::Type.coerce_to(name, String, :to_str)

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

  def printf(*args)
    if args.length._equal?(0)
      return nil
    end
    if (args[0]._kind_of?(IO))
      target = args.shift
    else
      target = $stdout
    end
    target.printf(*args)
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
    elsif ex_class._is_a?(Exception) ||
          (ex_class.respond_to?(:exception) && !ex_class._isSymbol)
      ex = ex_class.exception(message)
      ex.__signal
    else
      ex = TypeError.new("exception class/object expected: #{ex_class.inspect}")
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
    elsif ex_class.respond_to?(:exception) && !ex_class._isSymbol
      ex = ex_class.exception(message)
      ex.set_backtrace(stack)
      ex.__signal
    else
      ex = TypeError.new("exception class/object expected: #{ex_class.inspect}")
      ex.__signal
    end
  end

  def raise(msg)
    if msg._isString
      raise(RuntimeError, msg)
    elsif msg.respond_to?(:exception) && !msg._isSymbol
      # msg should be a subclass of Exception or
      #  an object that returns a new exception
      ex = msg.exception
      if (ex.__handler_active)
        ex.__reraise
      else
        ex.__signal
      end
    else
      ex = TypeError.new("exception class/object expected: #{msg.inspect}")
      ex.__signal
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
    RUBY.require(Maglev::Type.coerce_to(name, String, :to_str))
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
    ms = if timeout._equal?(nil)
      nil
    elsif timeout._isFixnum
      timeout * 1000
    else
      timeout = Maglev::Type.coerce_to(timeout, Float, :to_f)
      (timeout * 1000.0 ).to_int
    end

    if ms._isFixnum && ms < 0
      raise ArgumentError, 'Kernel.select, timeout not representable as Fixnum milliseconds >=0'
    end

    arrays = __select(reads, writes, errs, *[ ms ])
    return arrays if arrays._equal?(nil)
    arrays.map{|arr| Array(arr)}
  end

  # Preferred alternative to         class << self ; self ; end  .
  # Faster and no concurrency side effects because not
  # reexecuting a class body  (see Trac 745).
  def singleton_class
    self.__singleton_class  # See Object#__singleton_class
  end

  def split(pattern=nil, limit=MaglevUndefined)
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

  primitive_nobridge '__high_priority_sleep_ms', '_highPriorityWaitForMilliseconds:'
  primitive_nobridge '__sleep_ms', '_waitForMilliseconds:'

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
  primitive_nobridge '__forkv_exec', '_forkvExec:'

  def `(arg)                         #` close quote for Emacs higlighting
    # called from generated code
    arg = Maglev::Type.coerce_to(arg, String, :to_str)
    arr = __forkv_exec(arg)  #   raw_status is arr[0]
    # Note that arr is available as $?.__prim_result for debugging
    status = arr[1]
    unless status._equal?(0)
      Errno.raise_errno(status, arg)
    end
    arr[2]  # child stdout
  end

  def sub(pattern, replacement)
    str = self.__getRubyVcGlobal(0x21) # get callers $_
    if str._equal?(nil)
      raise TypeError, 'Kernel.sub , caller frame has no reference to $_ '
    end
    unless str._isString
      raise TypeError, '$_ is not a String'
    end
    replacement = Maglev::Type.coerce_to(replacement, String, :to_str)
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

  def __xstr_exec(arg)
    # called from generated code
    arg = Maglev::Type.coerce_to(arg, String, :to_str)
    arr = __forkv_exec(arg)  #   raw_status is arr[0]
    # Note that arr is available as $?.__prim_result for debugging
    arr[2]  # child stdout
  end

  # def system ; end  # see above near   def exec

  # See <tt>Signal.trap</tt>.
  def trap(sig, command=nil, &block)
    Signal.trap(sig, command, &block)
  end

  def __as_file(arg)
    if arg._is_a?(File)
      f = arg
    else
      fn = Maglev::Type.coerce_to(arg, String, :to_str)
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

  # Uses the integer +cmd+ to perform various tests on +file1+, or on
  # +file1+ and +file2+.
  def test(cmd, file1, file2=MaglevUndefined)
    if file2._equal?(MaglevUndefined)
      return self.test(cmd, file1)
    end
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

  # def throw(aSymbol, aValue); end  # implemented in smalltalk
  primitive_nobridge 'throw' , 'throw:with:'

  # def throw(aSymbol); end  # implemented in smalltalk
  primitive_nobridge 'throw' , 'throw:'

  primitive_nobridge '__trace_global_assign&', 'traceGlobalVarAssign:block:'

  def __call_tracevar_block(new_value, &block)
    # called from Smalltalk
    block.call(new_value)
  end

  def __trace_var(name, &block)
    unless name._isSymbol
      name = Maglev::Type.coerce_to(name, String, :to_str)
      name = name.to_sym
    end
    __trace_global_assign(name, &block)
  end

  def trace_var(name, ignored_cmd=MaglevUndefined , &block)
    # cmd is currently ignored
    # name must be a String or Symbol beginning with '$'
    # if block is nil, has no effect
    if block_given?
      __trace_var(name, &block)
    end
  end

  def untrace_var(name, ignored_cmd=MaglevUndefined)
    # cmd is currently ignored
    # name must be a String or Symbol beginning with '$'
    blk = nil  #  shut off tracing for specified name
    __trace_var(name, &blk)
  end

  # Generates a Continuation object, which it passes to the associated
  # block. Performing a cont.call will cause the callcc to return (as
  # will falling through the end of the block). The value returned by
  # the callcc is the value of the block, or the value passed to
  # cont.call. See class Continuation for more details.
  def callcc
    raise LocalJumpError, "no block given" unless block_given?
    Proc.new.__call_cc
  end
end
