# File Trac908a.rb 
#    coverage of a problem with constant resolution in method
#    defined via a class_eval
module Treetop
  module Runtime
    class SyntaxNode
    end
    class CompiledParser
    end
  end
end

Object.class_eval('
  module Less
    module StyleSheet
      module Common
        include Treetop::Runtime

        def badc
          SyntaxNode
        end
      end
    end
  end
')

module Less
  module StyleSheet
    module Common
      def okc
        SyntaxNode
      end
    end
  end
end

module Less 
  module StyleSheet
    include Treetop::Runtime
    include Common
  end
  class StyleSheetParser < Treetop::Runtime::CompiledParser
    include StyleSheet
  end 

end

ssp = Less::StyleSheetParser.new
ca = ssp.okc
unless ca.equal?(Treetop::Runtime::SyntaxNode) ; raise 'fail'; end
cb = ssp.badc
unless cb.equal?(Treetop::Runtime::SyntaxNode) ; raise 'fail'; end
true
