# Reduced Conflict Queue
# RCQueue is identically Smalltalk RcQueue

RCQueue = __resolve_smalltalk_global(:RcQueue)

# An RCQueue (reduced-conflict queue) is an implementation of a FIFO queue that
# provides significantly reduced concurrency conflicts when used in an
# environment with multiple producers (users that add elements to the queue) and
# a single consumer (a user that removes items from the queue).  Producers are
# guaranteed not to conflict with each other, nor with a single consumer.

# Unlike Array, RCQueue provides for concurrent handling of an individual
# instance by multiple sessions.  Any or all of those sessions can modify
# the single instance.  When that happens, RCQueue reduces (but does not
# eliminate) the transaction conflicts that can arise among those sessions
# when they attempt to commit the instance to GemStone.

class RCQueue
  include Enumerable

  class_primitive 'new', 'new'

  primitive 'first', 'peek'

  primitive 'size', 'size'
  alias length size

  primitive 'empty?', 'isEmpty'

  primitive 'each&', 'do:'

  primitive 'add', 'add:'
  alias push add
  alias << add
  alias enq add

  primitive '__remove_all', 'removeAll'
  alias clear __remove_all

  primitive 'remove', 'remove'
  alias pop remove
  alias shift remove
  alias deq remove

end

