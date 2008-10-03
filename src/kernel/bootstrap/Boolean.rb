# ---------------------------------
#  NilClass

class NilClass
   primitive '&', '_rubyAnd:'
   #  For receiver nil,  Or and Xor are the same
   primitive '^', '_rubyOr:'
   primitive '|', '_rubyOr:'

   primitive 'nil?' , '_rubyNilQ'
   primitive 'to_a' , '_ruby_to_a'

   primitive 'to_f' , '_ruby_to_f'
   primitive 'to_i' , '_ruby_to_i'
   primitive 'to_s' , '_ruby_to_s'

   def self.name
      # override Smalltalk name
      'NilClass'
   end

   def inspect
     "nil"
   end

end

#---------------------------------
#   NilTF.gs has implementation of Boolean>>class
#     which returns either TrueClass or FalseClass

class Boolean
  primitive 'not'

  primitive 'inspect', 'asString'
  primitive '^', '_rubyXor:'

  primitive '&', '_rubyAnd:'

  primitive '|' , '_rubyOr:'

  def to_s
    self == true ? "true" : "false"
  end
end
