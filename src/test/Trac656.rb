# Distilled from Psych
#
# MagLev can not resolve the Nodes constant in the class eval
module Psych
  module Nodes
    class Sequence
    end
  end

  class TreeBuilder
    class_eval "x = Nodes::Sequence"
  end
end
