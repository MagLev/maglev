class Array
    primitive 'length', 'size'
    primitive 'size'
    # note, <<  can't use smalltalk add: , it returns arg, not receiver
    primitive '<<', '_rubyAddLast:'
    primitive 'concat', 'addAll:'
    primitive 'remove_if_absent', 'remove:ifAbsent:'
    
    primitive 'first'
    primitive 'last'
    
    primitive 'dup', 'copy'
    primitive 'clone', 'copy'
    primitive 'clear', 'removeAll'
    primitive 'empty?', 'isEmpty'
    primitive '==', '='
    primitive 'hash'
    primitive 'push', '_rubyAddLast:'
    primitive 'remove_first', 'removeFirst'
    primitive 'remove_last', 'removeLast'
    primitive 'reverse'
    primitive 'sort!', 'sort'
    
    primitive '[]' , '_rubyAt:'
    primitive '[]' , '_rubyAt:length:'
    primitive 'slice', '_rubyAt:'
    primitive 'slice', '_rubyAt:length:'
    
    primitive 'at' , '_rubyAt:'

    primitive '[]=', '_rubyAt:put:'
    primitive '[]=', '_rubyAt:length:put:'
    
    primitive 'any?&', 'anySatisfy:'
    primitive '_all', 'allSatisfy:'
    primitive '_detect', 'detect:ifNone:'
    primitive '+', ','

    #  &  uses Smalltalk implementation because Ruby Hash does not
    #   support a removeKey:otherwise: in the Ruby API.
    primitive '&',  'rubyIntersect:'

    primitive 'inspect', 'printString'
    primitive 'include?', 'includes:'
    primitive 'member?', 'includes:'
    primitive 'sort_by2&', 'sortBy:'
    primitive 'reject!&', 'removeAllSuchThat:'
    primitive 'pack', 'rubyPack:'
    primitive 'insert_all', 'insertAll:at:'
    primitive 'select&', 'select:'
    primitive 'inject&', 'inject:into:'

    # replace written in Smalltalk so it can use copyFrom:to:into:startingAt prim
    primitive 'replace', 'rubyReplace:'
    
    self.class.primitive 'alloc', 'new:'
    def self.new(size=0, value=nil)
        inst = alloc(size)
        if value
          for i in (0..(inst.length-1))
            inst[i] = value
          end
        end
        inst
    end
    
    def delete(el)
      remove_if_absent(el, proc{return nil})
      return el
    end
    
    def all?(&b)
      _all(b)
    end
    
    def unshift(element)
        insert_all([element], 1)
        self
    end
    
    def shift
        unless empty?
            remove_first
        end
    end
    
    def pop
        unless empty?
            remove_last
        end
    end
    
    def find(&block)
        _detect(block, nil)
    end
    
    def sort
       d = dup
       d.sort!
       d
    end
    
    def reverse!
        replace(reverse)
    end
    
    def collect(&b)
        result = Array.new(length)
        i = 0
        while i < length
            result[i] = b.call(self[i])
            i += 1
        end
        result
    end
    alias map collect
    
    def collect!(&b)
        i = 0
        while i < length
            self[i] = b.call(self[i])
            i += 1
        end
        self
    end
    
    alias map! collect!
    
    def each(&b)
        i = 0
        while i < length
            b.call(self[i])
            i += 1
        end    
    end
    
    def each_index(&b)
        0.upto(size-1, &b)
    end
    
    def reverse_each(&b)
        i = length - 1
        while(i >= 0)
            b.call(self[i])
            i -= 1
        end
    end
    
    def partition(&b)
        t = []
        f = []
        i = 0
        while i < length
            el = self[i]
            if(b.call(el))
                t << el
            else
                f << el
            end
        end
        [t,f]
    end
    
    def join(s="")
        out = ""
        max = length - 1
        for i in (0..max)
            out << self[i].to_s
            out << s unless i == max
        end
        out
    end
    
    def inspect
        "[" + collect{|ea| ea.inspect}.join(", ") + "]"
    end
    
    def sort_by(&block)
        sort_by2{|a,b| block.call(a) <= block.call(b)}
    end
    
    def insert(idx, *args)
        insert_all(args, idx+1)
    end
    
    def index(el)
      i = 0
      sz = size
      while(i < sz)
        if(self[i] == el)
            return i
        end
        i += 1
      end
      nil
    end

    def rindex(el)
      i = size - 1
      while(i >= 0)
        if(self[i] == el)
            return i
        end
        i -= 1
      end
      nil
    end
    
    def grep(pattern)
        select{|ea| pattern === ea}
    end
    
    def to_a
      self
    end
    
    def uniq
      hash = {}
      ary = []
      j = 0
      lim = size
      while (j < lim) 
        el = self[j]
	unless hash.include? el
	   ary << el
	   hash[el] = el
	end
        j = j + 1
      end
      ary
    end
    
    def uniq!
      replace(uniq)
    end
    
    def flatten
      flatten_onto([])
    end
    
    def flatten_onto(output)
      j = 0
      lim = size
      while (j < lim)
        el = self[j]
        el.flatten_onto(output)
        j = j + 1
      end
      output
    end
    
    def *(obj)
        result = []
        obj.times{result.concat(self)}
        result
    end
    
    def -(arg)
      argSize = arg.size
      mySize = size
      default = Array.new
      h = Hash.new(default)
      res = Array.new
      j = 0
      while (j < argSize) 
        el = arg[j]
        h[el] = el
        j = j + 1
      end 
      j = 0
      while (j < mySize)
        el = self[j]
        if (h[el].equal?(default)) 
          res << el   
        end
        j = j + 1
      end
      res
    end
    
    def slice!(x, y = nil)
        if y
            result = self[x,y]
            self[x,y] = nil
        else
            result = self[x]
            self[x] = nil
        end
        result
   end
end

def Array(arg)
  arg.to_a rescue [arg]
end
