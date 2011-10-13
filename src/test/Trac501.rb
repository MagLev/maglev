a = %w{ a b c d }
a.insert(2,99)
raise 'Fail 1' unless a == ["a", "b", 99, "c", "d"]

a.insert(-2, 1, 2, 3)
raise 'Fail 2' unless a == ["a", "b", 99, "c", 1, 2, 3, "d"]
#################### Trac Info
# ID:         501
# Summary:    Array#insert not handling negative indices
# Changetime: 2009-04-27 18:56:47+00:00
###

#  From the method documentation:
#  
#  array.insert(index, obj...) → array
#  Inserts the given values before the element with the given index (which may be negative).
#  
#     a = %w{ a b c d }
#     a.insert(2, 99)         #=> ["a", "b", 99, "c", "d"]
#     a.insert(-2, 1, 2, 3)   #=> ["a", "b", 99, "c", 1, 2, 3, "d"]
#  
#  