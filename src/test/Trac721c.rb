# Rails defines a cattr_* set of macros like the ruby attr_*, but it allows
# access to class variables (@@var) rather than instance variables.
#
# See comments near bottom about maglev problem

######################################################################
# Class monkey patching code from Rails3 ActiveSupport
######################################################################
class Class
  def cattr_reader(*syms)
    syms.each do |sym|
      class_eval(<<-EOS, __FILE__, __LINE__ + 1)
        unless defined? @@#{sym}
          sx = self
          @@#{sym} = nil
        end

        def self.#{sym}
          r = @@#{sym}
          # nil.pause
          r
        end
      EOS

      class_eval(<<-EOS, __FILE__, __LINE__ + 1)
          def #{sym}
            r = @@#{sym}
            # nil.pause
            r
          end
      EOS
      end
  end

  def cattr_writer(*syms)
    syms.each do |sym|
      class_eval(<<-EOS, __FILE__, __LINE__ + 1)
        unless defined? @@#{sym}
          sx = self
          @@#{sym} = nil
        end

        def self.#{sym}=(obj)
          @@#{sym} = obj
        end
      EOS

      class_eval(<<-EOS, __FILE__, __LINE__ + 1)
          def #{sym}=(obj)
            @@#{sym} = obj
            # nil.pause
          end
      EOS
      self.send("#{sym}=", yield) if block_given?
    end
  end

  def cattr_accessor(*syms, &blk)
    cattr_reader(*syms)
    cattr_writer(*syms, &blk)
  end
end

module ActiveSupport
  module Concern
    def append_features(base)
      super
      if instance_variable_defined?("@_included_block")
        # @_included_block.call( base)
        base.class_eval(&@_included_block) 
      end
      fx1 = base.foo1
      raise "Fail X" unless fx1 == 'foo 1 initial value'
    end

    def included(base = nil, &block)
      @_included_block = block
    end
  end
end

######################################################################
# BEGIN BUG
######################################################################

module Pages
  extend ActiveSupport::Concern
  included { | mod |
    @@foo1 = 'foo 1 initial value' 
    sx = self 
    cattr_accessor( :foo1 )

    @@foo2 = 'foo 2 initial value' 
    cattr_accessor( :foo2 )
  }
end

class Base
  include Pages
end

puts "---------- begin tests --------------"
# MagLev can use cattr_accessor to set and get the attribute:
Base.foo1 = 10
raise unless Base.foo1 == 10    # as expected

# But the initial value is not recognized by MagLev
# p Base.foo2  # Maglev gets nil, rather than @@foo2
raise 'fail foo2 initial value' unless (bfx = Base.foo2) == 'foo 2 initial value'


