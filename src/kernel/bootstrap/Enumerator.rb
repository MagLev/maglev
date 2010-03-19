module Enumerable

  Fixnum__MAX = Fixnum::MAX  # used in Enumerable.rb
  
  class Enumerator
    # The current implementation does not use fibers or continuations
    include Enumerable

    def initialize(object, enum_sel, arg1, arg2)
      @ofs = 0
      @obj = object
      unless enum_sel._isSymbol
        raise TypeError, 'second arg must be a Symbol'
      end
      @enum_selector = enum_sel
      @extra_args = [arg1, arg2]
    end

    def initialize(object, enum_sel, arg1)
      @ofs = 0
      @obj = object
      unless enum_sel._isSymbol
        raise TypeError, 'second arg must be a Symbol'
      end
      @enum_selector = enum_sel
      @extra_args = [arg1]
    end

    def initialize(object, enum_sel)
      @ofs = 0
      @obj = object
      unless enum_sel._isSymbol
        raise TypeError, 'second arg must be a Symbol'
      end
      @enum_selector = enum_sel
      @extra_args = []
    end

    def initialize(object)
      self.initialize(object, :each)
    end

    def initialize(&block)
      # per specs, requires 1.8.8 or above
      raise ArgumentError, 'Enumerator from block not supported in 1.8.7'
    end

    def each(&block)
      unless block_given?
        return self
      end
      @obj.__send__( @enum_selector, *@extra_args, &block)   
    end


    def each_with_index(&block)
      unless block_given?
        return @obj.each_with_index() # return an Enumerator
      end
      n = -1
      @obj.__send__( @enum_selector, *@extra_args) { | elem | 
        n += 1
        block.call( elem , n)  # must be last statement in block
      }
    end

    def each_with_object(memo, &block)
      @obj.__send__( @enum_selector, *@extra_args) { | elem | 
        block.call( elem , memo )
      }
    end

    def next 
      raise RuntimeError, 'Enumerable::Enumerator#next is subclass responsibility'
    end

    def rewind
      @ofs = 0
      self
    end

    alias with_index  each_with_index

    alias with_object each_with_object
  end
end

