class Set
   # Set is identically  Smalltalk IdentitySet 

    primitive_nobridge '<<', 'add:'
    primitive_nobridge '*'
    primitive_nobridge '+'
    primitive_nobridge '-'
    primitive 'each&', 'do:'
    primitive 'length', 'size'

    primitive_nobridge '_includes', 'includes:'
    primitive_nobridge '_remove', 'removeIfPresent:'

    class_primitive 'new'

    def self.name
      # override Smalltalk name
      :Set
    end
    
    def inspect(touchedSet=nil)
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
