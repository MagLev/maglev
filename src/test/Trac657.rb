# From http://copypastel.com/rofl/A_Maglev_Store-y

module Foo
  def self.included(klass)

    klass.class_eval do
      @@var = "Access me"

      # metaclass
      class << self
        def var
          @@var
        end
      end
    end

  end
end

class Bar
  include Foo
end

y = Bar.var
unless y == 'Access me' ; raise 'error1'; end 

module Persistable
  def self.included(klass)
    klass.class_eval do
      @@store = [:foo]

      class << self
        include Enumerable
        def each(&block)
          @@store.each &block
        end
      end
    end
  end
end

class C
  include Persistable
end

aa = C.class_variables
unless aa.size == 1 && aa[0] == '@@store' ; raise 'error2'; end
bb = C.all?
unless bb._equal?(true) ; raise 'error3'; end
true
