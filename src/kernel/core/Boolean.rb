# ---------------------------------
#  NilClass 

class NilClass
	primitive '&', '_rubyAnd:'
#  For receiver nil,  Or and Xor are the same
#	primitive '^', '_rubyOr:'
#  method  for '|'  is committed into env1 by NilTF.gs

	primitive 'nil?' , '_rubyNilQ'
	primitive 'to_a' , '_ruby_to_a'

	primitive 'to_f' , '_ruby_to_f'
	primitive 'to_i' , '_ruby_to_i'
	primitive 'to_s' , '_ruby_to_s'
	
	def inspect
	  "nil"
    end
end

#---------------------------------
#  TrueClass, FalseClass
#     both are identical to Boolean for now

class TrueClass
    primitive 'not'
#   & , ^, |   methods are committed into env1 by NilTF.gs
    primitive 'inspect', 'asString'
    primitive '^', 'xor:'
end
