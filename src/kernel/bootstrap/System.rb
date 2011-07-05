module Maglev
  # Class Maglev::System is identically Smalltalk System
  #
  # Maglev::System provides access to many transaction and locking
  # facilities in MagLev.  Among the facilities are:
  # <ul>
  #  <li>Transaction management (commit, abort etc.)</li>
  #  <li>Persistent Shared Counters</li>
  #  <li>Access system counters (metrics)</li>
  #  <li>Miscellaneous other support</li>
  # </ul>
  #
  # == Persistent Shared Counters
  #
  # Maglev::System maintins a set of persistent shared counters.
  #
  # Persistent shared counters provide a means for multiple sessions to
  # share common integer values.  There are 128 persistent shared counters,
  # numbered from 1 to 128. The index of the first counter is 1.
  #
  # Each update to a persistent shared counter causes a roundtrip to the
  # stone process.  However reading the value of a counter is handled by
  # the gem (and its page server, if any) and does not cause a roundtrip to
  # the stone.
  #
  # Persistent shared counters are globally visible to all sessions on all
  # shared page caches.
  #
  # Persistent shared counters hold 64 bit values and may be set to any
  # signed 64 bit integer value.  No limit checks are done when
  # incrementing or decrementing a counter.  Attempts to
  # increment/decrement the counter above/below the minimum/maximum value
  # of a signed 64-bit integer will cause the counter to 'roll over'.
  #
  # Persistent shared counters are independent of database transactions.
  # Updates to counters are visible immediately and aborts have no effect
  # on them.
  #
  # The values of all persistent shared counters are written to the primary
  # database extent at checkpoint time.  Updates between checkpoints are
  # written to the transaction log by the stone.  Therefore the state of
  # the persistent shared counters is recoverable after a crash, restore
  # from backup, and restore from transaction logs.
  #
  # Persistent shared counter performance is affected by the stone
  # configuration option STN_COMMITS_ASYNC.  Setting this option to TRUE
  # will result in faster update performance because the stone will respond
  # to update requests after the tranlog write has been queued but before
  # it completes.  Operating in this mode leaves a small chance of losing
  # data should the stone or host machine crash after the tranlog write was
  # queued but before it completes.  If this value is set to FALSE, the
  # stone will only respond to update requests after the tranlog write has
  # completed."
  class System

    # Transaction support
    class_primitive '__commitTransaction', 'commitTransaction'
    class_primitive '__abortTransaction', 'abortTransaction'
    class_primitive '__beginTransaction', 'beginTransaction'

    # Attempts to update the persistent state of the Repository to include
    # changes made by this transaction.
    #
    # If the commit operation succeeds, then this method returns +true+,
    # and the current transaction's changes, if any, become a part of the
    # persistent Repository.  After the repository update, the session
    # exits the current transaction.  If the transaction mode is
    # <tt>:autoBegin</tt>, then the session enters a new transaction.  If
    # the transaction mode is <tt>:manualBegin</tt>, then the session
    # remains outside of a transaction.
    #
    # If conflicts prevent the repository update, then this method returns
    # +false+.  Call the <tt>transactionConflicts</tt> (TBD: not
    # implemented in ruby yet) method to determine the nature of the
    # conflicts.  If the session is outside of a transaction, then this
    # method raises the error <tt>#rtErrPrimOutsideTrans</tt>.
    #
    # This method also updates the session's view of GemStone.  If the
    # commit operation succeeds, then all objects in the session's view are
    # consistent with the current state of GemStone.  If the commit fails,
    # then this method retains all the changes that were made to objects
    # within the current transaction.  However, commits made by other
    # sessions are visible to the extent that changes in this transaction
    # do not conflict with them.
    #
    # Returns +true+ if commit was read-only or succeeded , +false+ if
    # there was a failure.
    def self.commit_transaction
      __commitTransaction
    end

    # Rolls back all modifications made to committed GemStone objects and
    # provides the session with a new view of the most recently committed
    # GemStone state.
    #
    # These operations are performed whether or not the session was
    # previously in a transaction.  If the transaction mode is set to
    # <tt>:autoBegin</tt>, then a new transaction is started.  If the
    # transaction mode is set to <tt>:manualBegin</tt>, then a new
    # transaction is not started.
    def self.abort_transaction
      __abortTransaction
    end

    # Starts a new transaction for the session.  An abort is done before
    # the new transaction is started - giving the session a new snapshot of
    # the repository.
    #
    # If any permanent objects had been written by the session, their state
    # is aborted.  This method returns the receiver (+System+).
    def self.begin_transaction
      __beginTransaction
    end

    # Raise an exception if specified temporary object is added to the
    # closure list during an attempt to commit. Useful in debugging errors
    # due to attempt to commit not-commitable objects or classes.  The
    # exception details will include the parent object which triggered the
    # add to closure list.  Takes a single argument, a not-committed
    # object.  An argument of +nil+ shuts off a previous trap .
    #
    # Should be used with <tt>maglev-ruby -d</tt>. After control returns
    # topaz prompt with an error use 'stack save' and then SEND of
    # Object>>findReferencesInMemory: to navigate upwards from the object
    # given by the error argument.
    class_primitive 'trap_add_to_closure_list', 'trapAddToClosureList:'

    # implementation is Object>>_objectForOop:  but putting class_prim directives
    # in Object or Fixnum may upset results of Object.singleton_methods ...
    class_primitive_nobridge '__object_for_oop', '_objectForOop:' # used by ObjectSpace

    # Return the value in the Session Temps structure at the given key.
    # Returns nil if not found.
    class_primitive 'session_temp' , '_sessionTempsAt:'

    # Set the value in the Session Temps structure at the given key to value.
    # Returns the value stored.
    class_primitive 'session_temp_put', '_sessionTempsAt:put:' # returns value stored

    # _processInfo includes getuid getgid, getpid, kill  and related functions
    class_primitive_nobridge '__process_info', '_processInfo:with:with:'

    # return a 4 element Array holding the elements of the struct tms result from
    #  clock_t times(struct tms *buffer)
    #
    # The Array elements are tms_utime, tms_stime, tms_cutime, tms_cstime,
    # and each element ia s Float in units of seconds.
    #
    #   tms_utime is the CPU time used while executing instructions in the
    #   user space of the calling process.
    #
    #   tms_stime is the CPU time used by the system on behalf of the
    #   calling process.
    #
    #   tms_cutime is the sum of the tms_utime and the tms_cutime of the
    #   child processes.
    #
    #   tms_cstime is the sum of the tms_stime and the tms_cstime of the
    #   child processes.
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

    # Returns a Fixnum representing the session of the sender.
    class_primitive_nobridge 'session_id', 'session'

    # Return a SmallInteger which is the number of sessions present in the
    # system, including the Symbol Gem, garbage collection sessions, but
    # not the page manager session.
    class_primitive_nobridge 'session_count', 'currentSessionCount'

    # Returns a formatted String containing, for each current GemStone
    # session, the session number and userId.
    #
    # This method requires SessionAccess privilege if there is more than
    # one session logged in.
    class_primitive_nobridge 'session_names', 'currentSessionNames'

    # Increments the persistent shared counter at index by the specified
    # amount.  Amount must be a Fixnum or LargeInteger. For a LargeInteger,
    # amount must be representable as a signed 64-bit integer (between
    # -9223372036854775808 and 9223372036854775807).
    #
    # Returns the new value of the counter after the increment.
    #
    # See class comments above for more information on persistent shared
    # counters.
    class_primitive_nobridge '__increment_pcounter', 'persistentCounterAt:incrementBy:'

    # Decrements the persistent shared counter at index by the specified
    # amount.  Amount must be a Fixnum or LargeInteger. For a LargeInteger,
    # amount must be representable as a signed 64-bit integer (between
    # -9223372036854775808 and 9223372036854775807).
    #
    # Returns the new value of the counter after the decrement.
    #
    # See class comments above for more information on persistent shared
    # counters.
    class_primitive_nobridge '__decrement_pcounter', 'persistentCounterAt:decrementBy:'

    # Get the value of the persistent counter +arg+.  +arg+ must be in
    # range 1 <= arg <= 128.
    #
    # See Maglev::System class comments for details on persistent counters.
    class_primitive_nobridge 'pcounter', 'persistentCounterAt:'

    # Sets the persistent shared counter at index to the specified
    # value.  value must be a SmallInteger or LargeInteger.
    # For a LargeInteger,  amount must be representable as a signed
    # 64-bit integer (between -9223372036854775808 and 9223372036854775807).
    #
    # Returns value, the new value of the counter.
    #
    # See Maglev::System class comments for details on persistent counters.
    class_primitive_nobridge 'set_pcounter', 'persistentCounterAt:put:'

    # Increments the persistent shared counter at index by the specified
    # amount.  Amount must be a SmallInteger or LargeInteger.  For a
    # LargeInteger, amount must be representable as a signed 64-bit integer
    # (between -9223372036854775808 and 9223372036854775807).
    #
    # Returns the new value of the counter after the increment.
    #
    # See Maglev::System class comments for details on persistent counters.
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
    # See Maglev::System class comments for details on persistent counters.
    def self.decrement_pcounter(counter, by=1)
      __decrement_pcounter(counter, by)
    end

    # Methods for looking at the size of Temporary Object Space (a major
    # contributer to the VM memory footprint)

    # Returns the approximate number of bytes of temporary object memory
    # being used to store objects.
    class_primitive 'temp_obj_space_used', '_tempObjSpaceUsed'

    # Returns the approximate maximum number of bytes of temporary object
    # memory which is usable for storing objects.
    class_primitive 'temp_obj_space_max', '_tempObjSpaceMax'

    # Returns the approximate percentage of temporary object memory which
    # is in use to store temporary objects.  This is equivalent to the
    # expression:
    #
    #    (System.temp_obj_space_used * 100) / (System.temp_obj_space_max)
    #
    # Note that it is possible for the result to be slightly greater than
    # 100%. This result indicates temporary memory is almost completely
    # full.
    class_primitive 'temp_obj_space_percent_used', '_tempObjSpacePercentUsed'

    # Returns the full network name of the stone this VM is connected to.
    class_primitive '__stone_name', 'stoneName'

    # Returns the name of the stone the VM is connected to.  If +full+ is
    # true, returns the full network name (e.g., "!TCP\#server!maglev").
    # If +full+ is false (default), returns just the stone name (e.g.,
    # "maglev").
    def self.stone_name(full=false)
      full ? __stone_name : __stone_name[/.*!(.*)$/,1]
    end

    #--
    # Support for statmonitor statistics
    #
    # See the GS64 System Administration Guide, Appendix G: "statmonitor
    # and VSD reference" for details on the statistics system.
    #++

    # Returns an Array whose contents are described by the result of the
    # _cache_statistics_description method.  The Array contains statistics
    # for the specified slot in the GemStone shared memory cache to which
    # this session is attached.
    #
    # The argument should be a Fixnum between 0 and the number of process
    # slots in the shared cache minus 1, inclusive.  If the argument is
    # outside the range of valid process slots, or the session executing
    # this method is not using a shared cache, generate an error.  If the
    # slot specified is an inactive slot, returns nil.  The method
    # _cache_slot_for_sssion_id may be used to determine the process slot
    # of a session on the same shared cache.
    #
    # The process slots that are predefined are:
    #
    #    slot 0: The shared page cache monitor.
    #
    #    slot 1: The Stone if the cache is on the same machine as the Stone.
    #            Otherwise, a page server that is used to monitor the cache for
    #            the Stone.
    #
    # No other slots are guaranteed.  However, slot 2 is the often the page server
    # and slot 3 is often the GcGem.  These depend to some extent on the relative
    # speed of the processes during startup.  In addition, the GcGem can be
    # shut down, and when it is restarted, it is unlikely to end up at the same
    # position.
    class_primitive_nobridge '_cache_statistics', 'cacheStatistics:'

    # Returns an Array of Strings describing the result of the method
    # _cache_tatistics.
    #
    # A new Array of new Strings is created every time this primitive is
    # called, so the application may wish to cache the result.
    class_primitive_nobridge '_cache_statistics_description', 'cacheStatisticsDescription'

    # Returns the process slot in the SharedPageCache that corresponds to
    # my process.  If the SharedPageCache is not in use, returns -1.
    class_primitive_nobridge 'my_cache_slot', 'myCacheProcessSlot'

    # Returns the cache statistics for the current session.
    class_primitive_nobridge 'my_cache_statistics', 'myCacheStatistics'


    # The system reserves 47 counters for use by the application.  The
    # *_session_stat methods access and modify these counters.
    class_primitive_nobridge '_session_stat_at', '_sessionCacheStatAt:'
    class_primitive_nobridge '_session_stat_at_put', '_sessionCacheStatAt:put:'

    class_primitive_nobridge '_increment_session_stat', '_sessionCacheStatAt:incrementBy:'
    class_primitive_nobridge '_decrement_session_stat', '_sessionCacheStatAt:decrementBy:'

    # Gets the value of the session statistic at the specified index (which
    # should be in the range 0 to 47).
    def self.get_session_stat(index)
      raise "Session stat index must be 0..47" unless (0..47).include? index
      _session_stat_at(index)
    end

    # Set the session statistic at the specified index (which should be in
    # the range 0 to 47) to the specified value i, which must be a Fixnum.
    def self.set_session_stat(index, value)
      raise "Session stat index must be 0..47" unless (0..47).include? index
      _session_stat_at_put(index, value)
    end

    # Increments the session statistic at +index+ by +delta+ (default 1).
    def self.increment_session_stat(index, delta=1)
      raise "Session stat index must be 0..47" unless (0..47).include? index
      _increment_session_stat(index, delta)
    end

    # Decrements the session statistic at +index+ by +delta+ (default 1).
    def self.decrement_session_stat(index, delta=1)
      raise "Session stat index must be 0..47" unless (0..47).include? index
      _decrement_session_stat(index, delta)
    end
  end
end
