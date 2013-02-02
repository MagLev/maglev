#--
# = timeout.rb ,  modified for loading during Maglev bootstrap
#
# execution timeout
#
# = Copyright
#
# Copyright:: (C) 2000  Network Applied Communication Laboratory, Inc.
# Copyright:: (C) 2000  Information-technology Promotion Agency, Japan
#
#++
#
# = Description
#
# A way of performing a potentially long-running operation in a thread, and
# terminating it's execution if it hasn't finished within fixed amount of
# time.
#
# Previous versions of timeout didn't use a module for namespace. This version
# provides both Timeout.timeout, and a backwards-compatible #timeout.
#
# = Synopsis
#
#   require 'timeout'
#   status = Timeout::timeout(5) {
#     # Something that should be interrupted if it takes too much time...
#   }
#

module Timeout

  # classes Error, ExitException  defined in timeout1.rb

  ##
  # Executes the method's block. If the block execution terminates before +sec+
  # seconds has passed, it returns true. If not, it terminates the execution
  # and raises +exception+ (which defaults to Timeout::Error).
  #
  # Note that this is both a method of module Timeout, so you can 'include
  # Timeout' into your classes so they have a #timeout method, as well as a
  # module method, so you can call it directly as Timeout.timeout().

  def timeout(sec, klass = nil, &block)
    if sec._equal?(nil)
      return block.call
    end
    if sec._isFixnum
      millisecs = sec * 1000
    else
      sec = Maglev::Type.coerce_to(sec, Float, :to_f)
      millisecs = (sec * 1000.0).to_int
    end
    unless millisecs._isFixnum
      raise  RangeError , 'Timeout.timeout arg * 1000 must be a Fixnum'
    end
    if millisecs._equal?(0)
      return block.call
    end
    raise ThreadError, "timeout within critical session" if Thread.critical
    # Maglev, optimization just use ExitException, not Class.new(ExitException)
    excls = klass || ExitException  
    begin
      x = Thread.current
      y = Thread.start {
        __high_priority_sleep_ms( millisecs )
        if x.alive?
          x.raise  excls, "execution expired" 
        end
      }
      block.call(sec)
      #    return true
    rescue excls => e
      # Maglev,  comment out code which apparently has no effect
      # rej = /\A#{Regexp.quote(__FILE__)}:#{__LINE__-4}\z/o
      # (bt = e.backtrace).reject! {|m| rej =~ m}
      # level = -caller(CALLER_OFFSET).size
      # while THIS_FILE =~ bt[level]
      #   bt.delete_at(level)
      #   level += 1
      # end
      raise if klass            # if exception class is specified, it
                                # would be expected outside.
      raise Error, e.message    #  e.backtrace  # Maglev, skip backtrace
    ensure
      y.kill if y and y.alive?
    end
  end

  module_function :timeout

end

# top level   def timeout(n, e = nil, &block) ; end # is defined in common/kernel.rb

