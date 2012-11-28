# MagLev version of ruby-debug
#
# This file implements the API defined by the ruby-debug gem.  It does not
# provide a command-level replacement for ruby-debug.  It does allow things
# uses such as "rspec -d ...".
#
# MagLev already has its own source level debugger built-in.  This is a
# wrapper for that.  Not all features work yet.
#
# 1. The debugger does not yet support running a file and stopping at the
#     first line of code.
#

module Debugger

  # Debugger.start(options) -> bool
  # Debugger.start(options) { ... } -> obj
  #
  # If it's called without a block it returns +true+, unless debugger
  # was already started.  If a block is given, it starts debugger and
  # yields to block. When the block is finished executing it stops
  # the debugger with Debugger.stop method.
  #
  # If a block is given, it starts debugger and yields to block. When
  # the block is finished executing it stops the debugger with
  # Debugger.stop method. Inside the block you will probably want to
  # have a call to Debugger.debugger. For example:
  #
  #     Debugger.start{debugger; foo}  # Stop inside of foo
  #
  # MagLev ignores the options
  def start(options={ }, &block)
    if block_given?
      Debugger.start
      yield
      Debugger.stop
    else
      true
    end
  end
  module_function :start

  # Stop the debugger (a no-op in MagLev).
  def stop
    # no-op
  end
  module_function :stop

  # Returns true iff the debugger is active.
  # MagLev raises NotImplementedException
  def started?
    raise NotImplementedException.new "Debugger.started? not implemented"
  end
  module_function :started?

  # Reads/runs the given file containing debugger commands
  # MagLev raises NotImplementedException
  def run_script(file, out = STDOUT)
    raise NotImplementedException.new "Debugger.run_script not implemented"
  end
  module_function :run_script

  # Returns $! from last exception, or nil
  # MagLev always returns nil.
  def last_exception
    nil
  end
  module_function :last_exception


  # MagLev raises NotImplementedException
  def context
    raise NotImplementedException.new "Debugger.context not implemented"
  end
  module_function :context

  # MagLev raises NotImplementedException
  def settings
    raise NotImplementedException.new "Debugger.settings not implemented"
  end
  module_function :settings
end

module Kernel

  # Invoke the debugger (drop down into topaz).  Performs +steps+
  # line-event steps before entering the debugger.
  #
  # MagLev does not support the steps option.
  def debugger(steps=nil)
    $stderr.puts "Warning: MagLev does not support debugger steps option. Ignoring steps." unless steps.nil?
    nil.pause
  end

  # Not implemented:
  #
  # Returns a binding of n-th call frame
  #
  # def binding_n(n = 0) ... end
end

# class Module
#   #
#   # Wraps the +meth+ method with Debugger.start {...} block.
#   #
#   def debug_method(meth)
#   end

#   #
#   # Wraps the +meth+ method with Debugger.post_mortem {...} block.
#   #
#   def post_mortem_method(meth)
#   end
# end


