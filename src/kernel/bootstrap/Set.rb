class Set
    primitive '<<', 'add:'
    primitive '*'
    primitive '+'
    primitive '-'
    primitive 'each&', 'do:'
    primitive 'length', 'size'
    class_primitive 'new'

    def self.name
      # override Smalltalk name
      :Set
    end
    
    def inspect
        "[[#{length}]]"
    end
    
    def group_by(&block)
        groups = {}
        each do |item|
            val = block.call(item)
            group = groups[val] ||= Set.new
            group << item
        end
        groups
    end
    
    def sum(&block)
        s = 0
        each{|e| s += block.call(e)}
        s
    end
    
    def avg(&block)
        sum(&block) / length
    end
end
