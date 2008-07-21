Random.class.primitive 'new'
Random.primitive 'next', 'nextInt:'
Random.primitive 'next'
RandomInstance = Random.new

class Object
    primitive '==', '='
    primitive 'halt'
    primitive 'hash'
    primitive 'object_id', 'asOop'
    
    #  not   is now a special selector, 
    #   GsComSelectorLeaf>>selector:  translates #not to bytecode Bc_rubyNOT 
    # primitive 'not'

    primitive 'nil?' , '_rubyNilQ'

    # inspect will print to topaz -l output or gem log if Omni client GUI
    #   not initialized
    primitive '_send', 'rubySend:withArguments:'
    
    primitive 'freeze', 'immediateInvariant'
    primitive 'respond_to?', 'rubyRespondsTo:'
    primitive 'print_line', 'rubyPrint:'
    primitive 'to_s', 'asString'

    # pause is not standard Ruby, for debugging only .
    #  trappable only by an Exception specifying exactly error 6001
    primitive 'pause', 'pause'
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
                if(arg.kind_of? Array)
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
    
    def __send__(sym, *args)
      _send(sym, args)
    end
    alias send __send__
    
    def gets(sep="\n")
        $stdin.gets(sep)
    end

    def format(str, *args)
        args.each{|a| str.sub!(/%(d|s)/, a.to_s)}
        str
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
end
