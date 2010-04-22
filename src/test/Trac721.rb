# Rails defines a cattr_* set of macros like the ruby attr_*, but it allows
# access to class variables (@@var) rather than instance variables.
#
# See comments near bottom about maglev problem

######################################################################
# Class monkey patching code from Rails3 ActiveSupport
######################################################################
class Class
  def cattr_reader(*syms)
#    options = syms.extract_options!
    syms.each do |sym|
      class_eval(<<-EOS, __FILE__, __LINE__ + 1)
        unless defined? @@#{sym}
          @@#{sym} = nil
        end

        def self.#{sym}
          @@#{sym}
        end
      EOS

 #     unless options[:instance_reader] == false
        class_eval(<<-EOS, __FILE__, __LINE__ + 1)
          def #{sym}
            @@#{sym}
          end
        EOS
      end
#    end
  end

  def cattr_writer(*syms)
 #   options = syms.extract_options!
    syms.each do |sym|
      class_eval(<<-EOS, __FILE__, __LINE__ + 1)
        unless defined? @@#{sym}
          @@#{sym} = nil
        end

        def self.#{sym}=(obj)
          @@#{sym} = obj
        end
      EOS

      #unless options[:instance_writer] == false
        class_eval(<<-EOS, __FILE__, __LINE__ + 1)
          def #{sym}=(obj)
            @@#{sym} = obj
          end
        EOS
      #end
      self.send("#{sym}=", yield) if block_given?
    end
  end

  def cattr_accessor(*syms, &blk)
    cattr_reader(*syms)
    cattr_writer(*syms, &blk)
  end
end

######################################################################
# BEGIN BUG
######################################################################

class Base
  @@foo1 = 'foo 1 initial value'
  cattr_accessor :foo1

  @@foo2 = 'foo 2 initial value'
  cattr_accessor :foo2
end

# MagLev can use cattr_accessor to set and get the attribute:
Base.foo1 = 10
raise unless Base.foo1 == 10    # as expected

# But the initial value is not recognized by MagLev
p Base.foo2  # Maglev gets nil, rather than @@foo2
raise 'fail foo2 initial value' unless Base.foo2 == 'foo 2 initial value'
