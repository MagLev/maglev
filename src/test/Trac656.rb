# Distilled from Psych
#
# MagLev can not resolve the Nodes constant in the class eval
module Psych
  module Nodes
    class Sequence
    end
  end

  class TreeBuilder
    sx = Nodes::Sequence
    xx = nil
    class_eval( "xx = Nodes::Sequence" )
    unless sx.equal?(xx) ; raise 'error'; end
  end
end

BX = 44
module M
  class A
    AX = 59
    BX = 66
  end

  class B
    o = A.new
    # BX = 77
    unless o.instance_eval("BX") == 66 ; raise 'error'; end
    o.instance_eval(" class C; def m; CCX ; end;end ")  
    o.instance_eval(" $w = C ");  # maglev still failing
    class << o
      puts C.inspect
      if defined?(Maglev) 
        unless (nx = C.inspect) == 'M::B::C' ; raise 'error'; end
      else
        puts C.inspect
      end
    end
    o.instance_eval(" class D; def m; DDX ; end;end")  
  end
  if defined?(Maglev)
    unless (wx = $w.inspect) == 'M::B::C' ; raise 'error'; end
  else
    puts $w.inspect
  end
end

module KernelSpecs
  class Bndx
    @@super_secret = "password"
    def initialize(n)
      @secret = n
    end
    def get_binding
      a = true
      @bind = binding
      # Add/Change stuff
      b = true
      @secret += 1
      @bind
    end
  end
end

bx = KernelSpecs::Bndx.new(99).get_binding
rx = eval("@@super_secret", bx)
unless rx == "password" ; raise 'error'; end

true
