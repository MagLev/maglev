class Range
    primitive 'each&', 'do:'
    primitive 'collect&', 'collect:'
    primitive 'to_a', 'asArray'
    primitive 'exclude_end?', 'excludeEnd'
    primitive '==', '='
    primitive 'hash'
    primitive 'first', '_from'
    primitive 'last', '_to'
    primitive 'length', 'size'
    
    def ===(n)
        return false if n < first
        return false if n > last
        if exclude_end?
            return false if n == last
        end
        return true
    end
    
    alias include? ===

    def initialize(fromArg, toArg)
      @from = fromArg
      @to = toArg
      @by = 1
      @excludeEnd = false
    end
end
