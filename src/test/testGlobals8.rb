class OptionParser
   NoArgument = [NO_ARGUMENT = :NONE, nil].freeze
   FooA = 99
   class OptionMap < Hash
     # include Completion
   end
   class Switch
       def initialize(pattern = nil, conv = nil,
                      short = nil, long = nil, arg = nil,
                      desc = ([] if short or long), block = Proc.new)
         raise if Array === pattern
         888
       end

     class NoArgument < self

       #
       # Raises an exception if any arguments given.
       #
       def parse(arg, argv)
	 yield(NeedlessArgument, arg) if arg
	 conv_arg(arg)
       end

       def self.incompatible_argument_styles(arg, t) # HACK was (*)
	 nil # HACK was empty
       end

       def self.pattern
	 Object
       end
     end
   end
   class List
     attr_reader :short
     def initialize
       @atype = {}
       @short = OptionMap.new
       @long = OptionMap.new
       @list = []
     end
   end
   ArgumentStyle = {}
   FooB = 99
   NoArgument.each {|el| ArgumentStyle[el] = Switch::NoArgument}
   FooC = 99
   DefaultList = List.new
   DefaultList.short['-'] = Switch::NoArgument.new {}
   DefaultList.short['x'] = Switch::NoArgument.new {throw :terminate}
   FooD = 99
end
