#
#   irb/workspace-binding.rb - 
#   	$Release Version: 0.9.5$
#   	$Revision: 11708 $
#   	$Date: 2007-02-13 08:01:19 +0900 (Tue, 13 Feb 2007) $
#   	by Keiju ISHITSUKA(keiju@ruby-lang.org)
#
# --
#
#   
#
module IRB
  class WorkSpace
    # create new workspace. set self to main if specified, otherwise
    # inherit main from TOPLEVEL_BINDING.
    def initialize(*main)
      if main[0].kind_of?(Binding)
	@binding = main.shift
      elsif IRB.conf[:SINGLE_IRB]
	@binding = TOPLEVEL_BINDING
      else
	case IRB.conf[:CONTEXT_MODE]
	when 0	# binding in proc on TOPLEVEL_BINDING
	  @binding = eval("proc{binding}.call",
		      TOPLEVEL_BINDING, 
		      __FILE__,
		      __LINE__)
	when 1	# binding in loaded file
	  require "tempfile"
	  f = Tempfile.open("irb-binding")
	  f.print <<EOF
	  $binding = binding
EOF
	  f.close
	  load f.path
	  @binding = $binding

	when 2	# binding in loaded file(thread use)
	  unless defined? BINDING_QUEUE
	    require "thread"
	    
	    IRB.const_set("BINDING_QUEUE", SizedQueue.new(1))
	    Thread.abort_on_exception = true
	    Thread.start do
	      eval "require \"irb/ws-for-case-2\"", TOPLEVEL_BINDING, __FILE__, __LINE__
	    end
	    Thread.pass
	  end
	  @binding = BINDING_QUEUE.pop

	when 3	# binging in function on TOPLEVEL_BINDING(default)
	  @binding = eval("def irb_binding; binding; end; irb_binding",
		      TOPLEVEL_BINDING, 
		      __FILE__,
		      __LINE__ - 3)
	end
      end
      if main.empty?
	@main = eval("self", @binding)
      else
	@main = main[0]
	IRB.conf[:__MAIN__] = @main
	case @main
	when Module
	  @binding = eval("IRB.conf[:__MAIN__].module_eval('binding', __FILE__, __LINE__)", @binding, __FILE__, __LINE__)
	else
	  begin 
	    @binding = eval("IRB.conf[:__MAIN__].instance_eval('binding', __FILE__, __LINE__)", @binding, __FILE__, __LINE__)
	  rescue TypeError
	    IRB.fail CantChangeBinding, @main.inspect
	  end
	end
      end
      eval("_=nil", @binding)
      eval("def method_missing(*args); if args.length == 1 then eval('@__'+args.first.to_s) else super(*args); end; end", @binding) # pretend to bind locals
    end

    attr_reader :binding
    attr_reader :main

    def evaluate(context, statements, file = __FILE__, line = __LINE__)
      # pretend to bind locals (really hide them in @__... vars)
      s2 = statements 
         #.gsub(/^ *([a-z_][a-z_0-9]*)*([-+*\/&|]*=|<<)([^=]|$)/i) {"@__#{$1}#{$2}#{$3}"} 
	 # Now done more robustly in ruby-lex.rb
      #p [:enter_workspace_evaluate,statements,s2,@binding]
      eval(s2, @binding, file, line)
      #p :leave_workspace_evaluate
      #result
    end
  
    # error message manipulator
    def filter_backtrace(bt)
      #p [IRB.conf[:CONTEXT_MODE],bt]
      case 1 #IRB.conf[:CONTEXT_MODE]
      when 0
	return nil if bt =~ /\(irb_local_binding\)/
      when 1
	if(bt =~ %r!/tmp/irb-binding! or
	   bt =~ %r!irb?/.*\.rb! or
	   bt =~ /irbx?\.rb/)
	  return nil
	end
      when 2
	return nil if bt =~ /irb?\/.*\.rb/
      when 3
	return nil if bt =~ /irb?\/.*\.rb/
	bt.sub!(/:\s*in `irb_binding'/){""} 
      end
      bt
    end

    def IRB.delete_caller
    end
  end
end
