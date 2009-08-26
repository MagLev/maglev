ObjectLogEntry = _resolve_smalltalk_global(:ObjectLogEntry)
class ObjectLogEntry
  # Returns the object log: a Collection of ObjectLogEntries.  The caller
  # is expected to abort, acquire lock and commit if necessary.
  class_primitive 'object_log', 'objectLog'

  class_primitive 'debug_level', 'debug'
  class_primitive '_debug', 'debug:object:'

  # Create a debug priority object log entry with the given message and
  # object.
  def self.debug(msg, obj=nil)
    _debug(msg, obj)
  end

  # Insert the receiver into the global object log.
  primitive_nobridge 'add_to_log', 'addToLog'

  # Return receiver's object attribute
  primitive_nobridge 'object', 'object'

  # Return receiver's priority attribute
  primitive_nobridge 'priority', 'priority'

  def to_s
    inspect
  end
end

OrderedCollection = _resolve_smalltalk_global(:OrderedCollection)
class OrderedCollection
  include Enumerable
  primitive 'each&', 'do:'
  primitive 'include?', 'includes:'
  primitive 'to_a', 'asArray'
  primitive '<<', 'add:'
  primitive 'length', 'size'
  primitive '_at', 'at:'
  def [](index)
    _at(index + 1)
  end
end
