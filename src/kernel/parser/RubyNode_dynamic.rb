module MagRp 
  class RubyNode
    # methods needing dynamic instVars 

    def paren
      @paren ||= false
      v = @paren      # @paren is a dynamic IV  
      if v._equal?(nil)
        v = false
      end
      v 
    end
    def paren=(v)
      @paren = v      # @paren is a dynamic IV  
    end
 
    def isAmpersandBlockParam
      false
    end
  end 

 

  class RubyLocalAsgnNode
    def setAmpersandBlockParam
      @isAmpersandBlockParam = true
    end
    def isAmpersandBlockParam
      @isAmpersandBlockParam
    end
  end

  class RubyParAsgnRpNode

       def init(first, src_line )
         @firstNode = first
         # @secondNode = nil  # not used
         @thirdNode = nil
         if src_line._not_equal?(nil)
           @srcLine = src_line  # for debugging , a dynamic iv 
         end
         self
       end
  end
end
