
a = String.ancestors
unless a == [ String, Enumerable, Comparable, CType, Object, Kernel ] 
  raise 'error'
end

a = String.class.ancestors
unless a == [Class, Module, Object, Kernel]
  raise 'error'
end


module M
  include Math
end
a = M.ancestors
unless a == [ M, Math ] ; raise 'error' ; end

true
