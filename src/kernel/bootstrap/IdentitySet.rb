class IdentitySet
   # Set is identically  Smalltalk IdentitySet

    primitive_nobridge '<<', 'add:'
    primitive_nobridge '*'
    primitive_nobridge '+'
    primitive_nobridge '-'
    primitive_nobridge '==' , '='
    primitive '_addall', 'addAll:'
    primitive 'each&', 'do:'
    primitive 'length', 'size'

    primitive_nobridge '_includes', 'includes:'
    primitive_nobridge '_remove', 'removeIfPresent:'

    class_primitive 'new' , 'new'

    def self.with_all(*array)
      o = new
      o._addall(*array)   
      o
    end

    def inspect(touchedSet=nil)
        "[[#{length}]]"
    end

    def group_by(&block)
        groups = {}
        each do |item|
            val = block.call(item)
            group = groups[val] ||= IdentitySet.new
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
