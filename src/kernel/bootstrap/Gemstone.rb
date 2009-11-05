# Class Gemstone is identically Smalltalk System
#
# == Persistent Shared Counters
#
# The Gemstone class maintins a set of persistent shared counters.
#
# Persistent shared counters provide a means for multiple sessions to share
# common integer values.  There are 128 persistent shared counters,
# numbered from 1 to 128 (the index of the first counter is 1.
#
# Each update to a persistent shared counter causes a roundtrip to the
# stone process.  However reading the value of a counter is handled by the
# gem (and its page server, if any) and does not cause a roundtrip to the
# stone.
#
# Persistent shared counters are globally visible to all sessions on all
# shared page caches.
#
# Persistent shared counters hold 64 bit values and may be set to any
# signed 64 bit integer value.  No limit checks are done when incrementing
# or decrementing a counter.  Attempts to increment/decrement the counter
# above/below the minimum/maximum value of a signed 64-bit integer will
# cause the counter to 'roll over'.
#
# Persistent shared counters are independent of database transactions.
# Updates to counters are visible immediately and aborts have no effect on
# them.
#
# The values of all persistent shared counters are written to the primary
# database extent at checkpoint time.  Updates between checkpoints are
# written to the transaction log by the stone.  Therefore the state of the
# persistent shared counters is recoverable after a crash, restore from
# backup, and restore from transaction logs.
#
# Persistent shared counter performance is affected by the stone
# configuration option STN_COMMITS_ASYNC.  Setting this option to TRUE will
# result in faster update performance because the stone will respond to
# update requests after the tranlog write has been queued but before it
# completes.  Operating in this mode leaves a small chance of losing data
# should the stone or host machine crash after the tranlog write was queued
# but before it completes.  If this value is set to FALSE, the stone will
# only respond to update requests after the tranlog write has completed."
#
class Gemstone

  # Transaction support
  class_primitive '__commitTransaction', 'commitTransaction'
  class_primitive '__abortTransaction', 'abortTransaction'
  class_primitive '__beginTransaction', 'beginTransaction'

  def self.commitTransaction
    __commitTransaction
  end
  def self.abortTransaction
    __abortTransaction
  end
  def self.beginTransaction
    __beginTransaction
  end

  # Raise an exception if specified temporary object is added to the
  # closure list during an attempt to commit. Useful in debugging 
  # errors due to attempt to commit not-commitable objects or classes. 
  # The exception details will include the parent object which triggered
  # the add to closure list.  
  # Takes a single argument, a not-committed object.  
  # An argument of nil shuts off a previous trap .
  #
  # Should be used with   maglev-ruby -d   . After control returns
  # topaz prompt with an error use 'stack save' 
  # and then SEND of Object>>findReferencesInMemory: to navigate upwards
  # from the object given by the error argument.
  class_primitive 'trap_add_to_closure_list', 'trapAddToClosureList:'

  class_primitive_nobridge '__object_for_oop', '_objectForOop:' # used by ObjectSpace
    # implementation is Object>>_objectForOop:  but putting class_prim directives
    # in Object or Fixnum may upset results of Object.singleton_methods ...

  class_primitive 'session_temp' , '_sessionTempsAt:'  # returns nil if not found
  class_primitive 'session_temp_put', '_sessionTempsAt:put:' # returns value stored

  # _processInfo includes getuid getgid, getpid, kill  and related functions
  class_primitive_nobridge '__process_info', '_processInfo:with:with:'
  class_primitive_nobridge '__host_times', '_hostTimes'

  # Return the real uid for the server process
  def self.getuid
    __process_info(0, nil, nil)
  end

  # Return the effective uid for the server process
  def self.geteuid
    __process_info(1, nil, nil)
  end

  # Return the real group id for the server process
  def self.getgid
    __process_info(2, nil, nil)
  end

  # Return the effective group id for the server process
  def self.getegid
    __process_info(3, nil, nil)
  end

  # Return the process id for the server process
  def self.getpid
    __process_info(8, nil, nil)
  end

  # Return the process group id for the server process
  def self.getpgrp
    __process_info(9, nil, nil)
  end

  # Return the parent process id for the server process
  def self.getppid
    __process_info(10, nil, nil)
  end

  class_primitive_nobridge '__increment_pcounter', 'persistentCounterAt:incrementBy:'
  class_primitive_nobridge '__decrement_pcounter', 'persistentCounterAt:decrementBy:'

  # Get the value of the persistent counter +arg+.  +arg+ must be in range 1 <= arg <= 128.
  #
  # See Gemstone class comments for details on persistent counters.
  class_primitive_nobridge 'pcounter', 'persistentCounterAt:'

  # Sets the persistent shared counter at index to the specified
  # value.  value must be a SmallInteger or LargeInteger.
  # For a LargeInteger,  amount must be representable as a signed
  # 64-bit integer (between -9223372036854775808 and 9223372036854775807).
  #
  # Returns value, the new value of the counter.
  #
  # See Gemstone class comments for details on persistent counters.
  class_primitive_nobridge 'set_pcounter', 'persistentCounterAt:put:'

  # Increments the persistent shared counter at index by the specified
  # amount.  Amount must be a SmallInteger or LargeInteger.  For a
  # LargeInteger, amount must be representable as a signed 64-bit integer
  # (between -9223372036854775808 and 9223372036854775807).
  #
  # Returns the new value of the counter after the increment.
  #
  # See Gemstone class comments for details on persistent counters.
  def self.increment_pcounter(counter, by=1)
    __increment_pcounter(counter, by)
  end

  # Decrements the persistent shared counter at index by the specified
  # amount.  amount.  Amount must be a SmallInteger or LargeInteger.  For a
  # LargeInteger, amount must be representable as a signed 64-bit integer
  # (between -9223372036854775808 and 9223372036854775807).
  #
  # Returns the new value of the counter after the decrement.
  #
  # See Gemstone class comments for details on persistent counters.
  def self.decrement_pcounter(counter, by=1)
    __decrement_pcounter(counter, by)
  end
end
