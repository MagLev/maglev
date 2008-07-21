#---------------------------------
# Symbol
#    Smalltalk Symbol and Ruby Symbol are identical classes
#    note in Ruby , superclass of Symbol is Object

class Symbol
	primitive 'all_symbols', '_rubyAllSymbols'
	primitive 'id2name', 'asString'
        primitive '==', '='
        primitive 'hash'
	primitive 'inspect', 'asSymbol'
	primitive 'to_i', 'asOop'
	primitive 'to_int', 'asOop'
	primitive 'to_s', 'asString'
	primitive 'to_sym', 'asSymbol'
	
	def inspect
	   ":" + to_s
    end 
    
    def call(value)
        value.__send__(self)
    end
end
