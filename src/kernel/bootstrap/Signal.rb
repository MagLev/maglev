module Signal

  # following 3 installed by RubyContext>>_initTransient:
  # TrappableByName = { }  # transient Hash , keyed on signal names without 'SIG' prefix
  # TrappableSignals = [ ] # transient Array , indexed by signal number
  # TrappedSignals = { }   # transient IdentityHash,  key is signal number, value is block

  primitive_nobridge '__trap_signal', '_trapSignal:ignore:block:'

  module_function( :__trap_signal )

  # If a signal is handled as a result of a Signal#trap, 
  # then the VM does not chain to any previously installed handlers for 
  # that signal.  During execution of a Block or Proc installed as
  # a signal handler,  handling of other signals eligible to 
  # be handled by Signal#trap is deferred until that block completes.
  #
  def self.trap(sig, command=nil, &block)
    tnames = TrappableByName
    tsigs = TrappableSignals
    if sig._isFixnum
      if sig < 1
        raise ArgumentError, 'signal numbers must be >= 1'
      end
      name = TrappableSignals[sig]
      if name._equal?(nil)
        raise ArgumentError, 'signal #{sig} is not trappable from Ruby' 
      end
      num = sig
    else
      str = Type.coerce_to(sig, String, :to_str)
      if str[0,3] == 'SIG'
        str = str[3, str.__size - 3]
      end
      num = TrappableByName[str]
      if num._equal?(nil) 
        raise ArgumentError, 'signal #{sig} is not trappable from Ruby' 
      end
    end
    ignore = nil 
    blk = nil
    if command._equal?(nil)  
      if block_given?
        command = block
      end 
    elsif block_given?
      raise ArgumentError, "both command and block passed to Signal#trap"
    end
    if command._isString
      if command == '' || command == 'IGNORE' || command == 'SIG_IGN' 
        ignore = 1
      elsif command == 'DEFAULT' || command == 'SIG_DFL'
        ignore = 2  # want VM default,  blk left as nil
      elsif command == 'EXIT'
        ignore = nil
        blk = Proc.new { Kernel.exit(0) }
      else
        raise ArgumentError, 'Signal#trap, unsupported String command #{command}'
      end
    elsif command._isBlock || command._kind_of?(Proc)
      ignore = nil
      blk = command
    else
      raise ArgumentError, 'Signal#trap, command must be a String Proc, or Block'
    end
    TrappedSignals[num] = blk
    self.__trap_signal(num, ignore, blk)
  end

  def self.list
    TrappableByName
  end
end
