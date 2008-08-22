class Hash

    primitive '[]', 'at:'
    primitive '[]=', 'at:put:'

    primitive 'dup', 'copy'
    primitive 'default'
    primitive 'default&' , 'default:'
    primitive 'default=', 'setDefaultValue:'

    primitive 'default_proc' , 'defaultBlock'

    primitive 'delete', 'removeKey:'
    primitive 'delete&', 'removeKey:with:'

    primitive 'each&', 'keysAndValuesDo:'

    primitive 'hash'
    primitive '==', '='

    primitive 'length', 'size'

    primitive  'store', 'at:put:'
    
    primitive 'keys', 'keys'
    
    primitive 'has_key?', 'includesKey:'
    primitive 'inspect', 'printString'
    # primitive 'include?' 'includesKey:'

    def include?
      has_key?
    end
    
    def merge(hash)
        dup.update(hash)
    end
        
    def update(hash)
        hash.each{|k,v| self[k] = v}
        self
    end
    self.class.primitive 'new'
    self.class.primitive 'new', 'new:'
    self.class.primitive 'new&', 'new:'

    def replace(hash)
        keys.each{|k| delete k}
        update(hash)
    end

    def inspect
        return "{}" if length == 0
        str = "{"
        each{|k,v| str << k.inspect; str << "=>"; str << v.inspect; str << ", "}
        str[0..(str.length - 3)] + "}"
    end
end

class Struct
end
