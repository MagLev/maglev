# Note the duplicated '_' in the params list to the sort block.
# MagLev complains about it.

methods = [["b2", "ab", "c2", "e2", "d2"],
           ["b1", "a1", "c1", "d1", "e1"]]

expected = [["b1", "a1", "c1", "d1", "e1"],
            ["b2", "ab", "c2", "e2", "d2"]]

x = methods.sort_by do |_, k, a, _, m|
  [k, a, m].compact
end

raise "Fail #{methods.inspect}" unless x == expected

# Behavior = __resolve_smalltalk_global(:Behavior)

# class Behavior
#   primitive_nobridge '__source_code_for', 'sourceCodeAt:environmentId:'
#   def source_code_for(sym)
#     __source_code_for(sym, 1)
#   end
# end

# class C
#   def m
#     puts "A method"
#   end
# end

# # This one works (ends in "end")
# puts C.source_code_for(:m)

# # But this one, and many others, doesn't work:
# puts Math.source_code_for(:'asinh:')
