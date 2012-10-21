# Reduced Conflict Counter
# RCCounter is identically Smalltalk RcCounter

RCCounter = __resolve_smalltalk_global(:RcCounter)

# RCCounter is a simple counter that allows multiple users to update it
# without running into conflicts. It replaces the need for a mutex in
# normal Ruby programming when implementing a counter that would be accessed
# by multiple threads.
#
# Unlike another counter, RCCounter provides for concurrent handling of an individual
# instance by multiple sessions.  Any or all of those sessions can modify
# the single instance.  When that happens, RCCounter reduces (but does not
# eliminate) the transaction conflicts that can arise among those sessions
# when they attempt to commit the instance to GemStone.

class RCCounter
  class_primitive 'new', 'new'

  primitive 'value', 'value'
  primitive 'increment', 'increment'
  primitive 'increment_by', 'incrementBy:'
  primitive 'decrement', 'decrement'
  primitive 'decrement_by', 'decrementBy:'
end
