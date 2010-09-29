Behavior = __resolve_smalltalk_global(:Behavior)

class Behavior
  primitive_nobridge '__source_code_for', 'sourceCodeAt:environmentId:'
  def source_code_for(sym)
    __source_code_for(sym, 1)
  end
end

class C
  def m
    puts "A method"
  end
end

# This one works (ends in "end")
puts C.source_code_for(:m)

# But this one, and many others, doesn't work:
puts Math.source_code_for(:'asinh:')
