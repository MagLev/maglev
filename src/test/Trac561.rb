class Bar
end
module M
  class Foo
    CF = 6
  end
  class ::Bar
    CB = 5
  end
end      
unless Bar::CB == 5 ; raise 'error'; end

#-------
    o = Object.new
    class << o
      module FooO
        Bar = 561
      end
      class Baz; include FooO ; end
      class Baz;
        def self.bar;  Bar ; end
      end
      def baz; Baz; end
    end
    xx = o.baz.bar
    unless xx == 561 ; raise 'error'; end

true
