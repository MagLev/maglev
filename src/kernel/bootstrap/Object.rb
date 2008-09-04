Random.class.primitive 'new'
Random.primitive 'next', 'nextInt:'
Random.primitive 'next'
RandomInstance = Random.new

class Object
    # Begin private helper methods

    primitive '_kindBlkStrRanRegAry', '_rubyKind_Block_String_Range_Regexp_Array'
    #       results are                          0x10   0x8   0x4    0x2    0x1  else nil

    # _isInteger allows integer?  special sends to non-Numeric objects
    primitive '_isInteger', '_isInteger'

    # End private helper methods

    primitive '==', '='
    primitive 'halt'
    primitive 'hash'
    primitive 'object_id', 'asOop'

    #  not   is now a special selector,
    #   GsComSelectorLeaf>>selector:  translates #not to bytecode Bc_rubyNOT
    # primitive 'not'

    primitive 'nil?' , '_rubyNilQ'

    # rubySend: methods implemented in .mcz
    #   note special handling of '_send:' in  installPrimitive:selector:
    #  TODO: reduce indirections in send implementation, and make use of
    #   rubySend:
    primitive '_send:*', 'rubySend:withArguments:'

    def send(sym, *args)
      _send(sym, *args)
    end

    # primitive '_send:',    'rubySend:'
    # def send(aSym)
    #  _send(aSym)
    #end
    
    primitive 'freeze', 'immediateInvariant'
    # TODO:  fix inefficency in rubyRespondsTo: which is implemented in .mcz
    primitive 'respond_to?', 'rubyRespondsTo:'
    primitive 'print_line', 'rubyPrint:'
    primitive 'to_s', 'asString'

    # pause is not standard Ruby, for debugging only .
    #  trappable only by an Exception specifying exactly error 6001
    primitive 'pause', 'pause'

    #                                   rubyInspect comes from .mcz
    primitive 'inspect', 'rubyInspect'
    primitive 'method', 'rubyMethod:'

    def rand(n=nil)
        if n
            RandomInstance.next(n)
        else
            RandomInstance.next
        end
    end


    def to_str
        to_s
    end

    def to_a
       # remove this method for MRI 1.9 compatibility
       [ self ]
    end

    def loop
        raise LocalJumpError, "no block given" unless block_given?

        while true
            yield
        end
    end

    def block_given?
        true
    end

    def initialize(*args)
        self
    end

    def print(*objs)
      objs.each {|obj| $stdout << obj.to_s}
      nil
    end

    def printf(pattern, *args)
        puts pattern.gsub(/%s/){args.shift}
    end

    def puts(*args)
        if args.empty?
            $stdout << "\n"
        else
            args.each do |arg|
                knd = arg._kindBlkStrRanRegAry
                if ( knd.equal?(1) )  # if arg.kind_of?(Array)
                    puts *arg
                else
                    $stdout << arg.to_s
                    $stdout << "\n"
                end
            end
        end
        nil
    end

    def p(obj)
        puts obj.inspect
    end

    def ===(obj)
        self == obj
    end
 
    def gets(sep="\n")
        $stdin.gets(sep)
    end

    def sprintf(str, *args)
        format(str, *args)
    end

    def raise(err, str)
        err ||= RuntimeError
        err.signal(str)
    end

    def eval(str)
        RUBY.module_eval(str, Object)
    end

  self.class.primitive 'basic_module_function', 'rubyModuleFunction:'
  def self.module_function(*names)
    if names.length > 0
        names.each{|name| basic_module_function(name)}
    else
        @use_module_functions = true
    end
  end

  def self.const_defined?(c) false; end
  self.class.primitive 'include', 'addRubyVirtualSuperclass:'

  def extend(mod)
  end

  def at_exit
  end

  def to_fmt
    to_s
  end

  def flatten_onto(output)
    output << self
    output
  end

  def pretty_inspect; inspect; end

  require 'kernel/bootstrap/Kernel.rb'
end
