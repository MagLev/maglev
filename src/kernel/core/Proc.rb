# ---------------------------------
#   Proc 

def lambda(&b)
  b
end

class Proc
    primitive 'call', 'value'
    primitive 'call', 'value:'
    primitive 'call', 'value:value:'
    primitive 'call', 'value:value:value:'
    
    def inspect
      "#<Proc>"
    end
    
    def [](*args)
      case args.size
        when 0
          call
        when 1
          call(args[0])
        when 2
          call(args[0], args[1])
      end
    end
end
