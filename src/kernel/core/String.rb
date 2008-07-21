class String

    primitive '[]' , '_rubyAt:'
    primitive '[]' , '_rubyAt:length:'
    primitive 'slice', '_rubyAt:'
    primitive 'slice', '_rubyAt:length:'

    primitive '[]=', '_rubyAt:put:'
    primitive '[]=', '_rubyAt:length:put:'

    primitive '==', '='
    primitive 'hash'
    primitive 'length', 'size'
    primitive 'size', 'size'
    primitive 'size=', 'size:'
    
    # note smalltalk addAll:  returns arg, not receiver
    primitive '<<', '_rubyAddAll:'

    primitive '+', ','

    primitive 'replace', '_rubyReplace:'
    primitive 'substring1', 'copyFrom:to:'
    primitive '_strip', 'withBlanksTrimmed'
    primitive 'to_f', 'asFloat'
    primitive 'inspect', 'printString'
    primitive 'empty?', 'isEmpty'
    primitive 'dup', 'copy'
      # need to remove method  unpack:  from .mcz
    primitive 'unpack', 'rubyUnpack:'
    primitive '_findStringStartingAt', 'findString:startingAt:'
    primitive 'concat', '_rubyAddAll:'
    primitive 'reverse', 'reverse'
    
    def signal
        raise RuntimeError, self
    end
    
    def include?(item)
        !self.index(item).nil?
    end
    
    def index(item, offset=nil)
        if offset.nil?
            item._index_string(self, 0)
        else
            offset = size + offset if offset < 0
            string = self[offset..-1]
            return item._index_string(string, offset)
        end
    end
    
    def _index_string(string)
        string._findStringStartingAt(self, 1) - 1
    end
    
    def strip
        _strip
    end
    
    def gsub(regex, str, &block)
       out = ""
        start = 1
        regex.to_rx.each_match(self) do |match|
            out << substring1(start, match.begin(0))
            if block
                out << block.call.to_s
            else
                out << str
            end
            start = match.end(0) + 1
        end
        if start <= length
            out << substring1(start, length)
        end
        out
    end
    
    def sub(regex, str, &block)
        if match = regex.to_rx.match(self)
           out = ""
           out << self[0...(match.begin(0))]
           if block
                out << block.call.to_s
            else
                out << str
            end
           out << ((self[(match.end(0))...length]) || "")
           out
        else
            dup
        end    
    end
    
    def sub!(regex, str, &block)
        new = sub(regex, str, &block)
        if new == self
            nil
        else
            replace(new)
            self
        end
    end
    
    def scan(regex)
        result = []
        regex.to_rx.each_match(self) do |m|
            result << m[0]
        end
        result
    end
    
    # asUppercase is a smalltalk primitive
    primitive 'upcase', 'asUppercase'

    # asLowercase is a smalltalk to:do: loop in CharacterCollection
    primitive 'downcase', 'asLowercase'

    primitive '_to_i', 'asInteger'
    #  non-base-10 radix Ruby syntax not supported yet
    primitive '_to_i', '_asInteger:'
    
    def to_i
        _to_i
    rescue Exception
        0
    end
    
    def hex
        _to_i(16)
    end
    
    def *(n)
        str = ""
        n.times{str << self}
        str
    end
    
    def ljust(n)
        self
    end
    
    def to_s
        self
    end
    
    def to_rx
      Regexp.new(self)
    end
    
    def each_char
        each_byte do |b|
            temp = ' '
            temp[0] = b
            yield temp
        end
    end

    def each_byte
        0.upto(size-1) do |idx|
            yield self[idx]
        end        
    end
    
    def each_line(&b)
       /(.*)/.all_matches(self).each do |match|
        str = match[1]
        b.call(str)
       end
    end
    
    def downcase!
        replace(downcase)
    end
    
    def gsub!(regex, str, &block)
        replace(gsub(regex, str, &block))
    end
    
    def strip!
        replace(strip)
    end
    
    def chop!
        replace(chop)
    end
    
    def chop
        self[0..size-2]
    end
    
    def chomp
      if self[-1] == ?\r
        return self[0..-2]
      end
      
      if self[-1] == ?\n
        if self[-2] == ?\r
          return self[0..-3]
        else
          return self[0..-2]
        end
      end
      
      return self
    end
    
    def chomp!
      replace(chomp)
    end
    
    def _split_string(string, limit)
        Regexp.new(self)._split_string(string, limit)
    end
    
    def split(pattern=nil, limit=nil)
        return [] if empty?
        pattern ||= ($; || " ")
        
        pattern._split_string(self, limit)
    end
    
    def %(args)
        a = Array(args).dup
        gsub(/%(\d|\.)*./){a.shift.to_fmt}
    end
    
    def reverse!
        replace(reverse)
    end
    
    def tr!(from, to)
        map = []
        if from[0] == ?^
            for i in 0..255
                unless(from.include? i)
                    map[i] = to[0]
                end
            end
        else
            if from[1] == ?- && from.size == 3
                start = from[0]
                max = from[2]
                if to[1] == ?- && to.size == 3
                    offset = to[0] - start
                    last = to[2]
                    for i in start .. max
                        n = i + offset
                        n = last if n > last
                        map[i] = n
                    end
                else
                    for i in start .. max
                        map[i] = to[i - start] || to[-1]
                    end
                end
            else
                for i in 0 ... from.size
                    map[from[i]] = to[i] || to[-1]
                end
            end
        end
        
        for i in 0 ... size
            if c = map[self[i]]
                self[i] = c
            end
        end     
        self             
    end 
    
    def tr(from, to)
        dup.tr!(from, to)
    end
end

class Integer
    def _split_string(string, limit)
        self.chr._split_string(string, limit)
    end
    
    def _index_string(string, offset)
        i = 0
        string.each_byte do |ea|
            if ea == self % 256
                return i + offset 
            end
            i += 1
        end
        nil
    end
end


class Regexp
    def _index_string(string, offset)
        md = self.match(string)
        return nil if md.nil?
        md.begin(0) + offset 
    end

    def _split_string(string, limit)
        result = []
        if self.source == ""
          for i in 0...string.size
            result[i] = string[i, 1]
          end
        else        
          start = 0
          self.all_matches(string).each do |match|
              result << string[start...match.begin(0)]
              start = match.end(0)
          end
          if(start < string.length)
              result << string[start...string.length]
          end
        end
        result
    end
end
