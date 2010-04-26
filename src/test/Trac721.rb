# Rails defines a cattr_* set of macros like the ruby attr_*, but it allows
# access to class variables (@@var) rather than instance variables.
#
# See comments near bottom about maglev problem

######################################################################
# Class monkey patching code from Rails3 ActiveSupport
######################################################################
class Class
  Ccx = 45
  def cattr_reader(*syms)
    syms.each do |sym|
      puts "Doing class_eval for #{sym} "
      class_eval(<<-EOS, __FILE__, __LINE__ + 1)
        unless defined? @@#{sym}
          @@#{sym} = nil
        end

        def self.#{sym}
          @@#{sym}
        end
        def self.ccx
          # (sys =  Maglev::System).session_temp_put(:TrapResolve, true)
           r = Ccx
          # sys.session_temp_put(:TrapResolve, false)
           # nil.pause # expect 99
           r
        end
      EOS

      class_eval(<<-EOS, __FILE__, __LINE__ + 1)
        def #{sym}
          @@#{sym}
        end
      EOS
    end
  end

  def cattr_writer(*syms)
    syms.each do |sym|
      class_eval(<<-EOS, __FILE__, __LINE__ + 1)
        unless defined? @@#{sym}
          @@#{sym} = nil
        end

        def self.#{sym}=(obj)
          @@#{sym} = obj
        end
      EOS

      class_eval(<<-EOS, __FILE__, __LINE__ + 1)
	def #{sym}=(obj)
	  @@#{sym} = obj
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

# Maglev::System.session_temp_put(:TrapCv, true)
class Base
  Ccx = 99
  @@foo1 = 'foo 1 initial value'
  cattr_accessor :foo1

  @@foo2 = 'foo 2 initial value'
  cattr_accessor :foo2

   def self.ccx
  #Maglev::System.session_temp_put(:TrapResolve, true)
     r = Ccx
   #  nil.pause # expect 99
     r
   end
end

class C
  cattr_accessor :foo2
end

# MagLev can use cattr_accessor to set and get the attribute:
Base.foo1 = 10
raise unless Base.foo1 == 10    # as expected

# But the initial value is not recognized by MagLev
bcls = Base
bcxa = Base.ccx
bcxb = Base::Ccx
bcxc = Base.class::Ccx
puts "Base.ccx = #{bcxa} #{bcxb} #{bcxc}"
fx = Base.foo2  # Maglev gets nil, rather than @@foo2
unless [ bcxa, bcxb, bcxc] == [ 99, 99, 45 ] ; raise 'error in consts'; end
ccx = C.ccx
puts "C.ccx = #{ccx}"
unless ccx == 45 ; raise 'error ccx'; end
unless fx == 'foo 2 initial value'; raise 'fail foo2 initial value' ; end
puts 'ok'
