# Trac 708, failure during AST to IR in computeLastLineNumber
module Haml
  module Helpers

    def block_is_haml?(block)
      eval('_hamlout', block.binding)
      true
    rescue
      false
    end
  end
end
true
