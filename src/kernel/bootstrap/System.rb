# -*- coding: utf-8 -*-
module Maglev
  # Class Maglev::System is identically Smalltalk System
  #
  # Maglev::System provides access to many transaction and locking
  # facilities in MagLev.  Among the facilities are:
  # <ul>
  #  <li>Transaction management (commit, abort etc.)</li>
  #  <li>Persistent Shared Counters</li>
  #  <li>Object Locks</li>
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
  #
  # == Object Locks
  #
  # MagLev allows setting and releasing locks on objects.  The system
  # supports read locks and write locks. A session may hold only one kind
  # of lock on an object at a time.  Holding locks prevents transactions
  # whose activities would conflict with your own from committing changes
  # to the repository.  MagLev permits you to request any kind of lock,
  # regardless of your transaction mode or whether you are in a
  # transaction.
  #
  # If you request a lock on an object and another session already holds a
  # conflicting lock on it, then MagLev denies your request; MagLev
  # does not automatically wait for locks to become available.
  #
  # It is possible that you may never succeed in acquiring a lock, no
  # matter how long you wait. Furthermore, because GemStone does not
  # automatically wait for locks, it does not attempt deadlock
  # detection. It is your responsibility to limit the attempts to acquire
  # locks in some way.
  #
  # Locks work across sessions (i.e., locking an object in one VM, locks it
  # in all VMs connected to the same repository).
  #
  # For more information on locks, see the GemStone Programming Guide.
  #
  # === Read Locks
  #
  # Holding a read lock on an object means that you can use the object’s
  # value, and then commit without fear that some other transaction has
  # committed a new value for that object during your transaction. Another
  # way of saying this is that holding a read lock on an object guarantees
  # that other sessions cannot
  # * acquire a write lock on the object, or
  # * commit if they have written the object.
  #
  # === Write Locks
  #
  # Holding a write lock on an object guarantees that you can write the
  # object and commit. That is, it ensures that you won’t find that someone
  # else has prevented you from committing by writing the object and
  # committing it before you, while your transaction was in progress.
  # Another way of looking at this is that holding a write lock on an
  # object guarantees that other sessions cannot:
  # * acquire either a read or write lock on the object, or
  # * commit if they have written the object.
  #
  # Write locks differ from read locks in that only one session can hold a
  # write lock on an object. In fact, if a session holds a write lock on an
  # object, then no other session can hold any kind of lock on the
  # object. This prevents another session from receiving the assurance
  # implied by a read lock: that the value of the object it sees in its
  # view will not be out of date when it attempts to commit a transaction.
  #
  # === Non Lockable Objects
  #
  # MagLev does not allow you to lock instances of +Fixnum+, +Float+,
  # +TrueClass+, +FalseClass+ or nil (i.e., instances of Smalltalk classes
  # +SmallInteger+, +Boolean+, +SmallDouble+ or nil). Trying to lock these
  # special objects is meaningless.
  #
  # === Example
  #
  # In one MagLev VM, run the following code:
  #
  #     Maglev::System.write_lock Maglev::PERSISTENT_ROOT
  #     puts "grabed lock...sleeping..."
  #     sleep 10
  #
  #     Maglev::System.remove_locks_for_session
  #     puts "released locks...sleeping..."
  #     sleep 10
  #
  # In another MagLev VM, run the following code:
  #
  #     while true
  #       begin
  #         puts "Attempting to grab lock..."
  #         Maglev::System.write_lock Maglev::PERSISTENT_ROOT
  #         puts "Got the lock!"
  #         sleep 1
  #         Maglev::System.remove_lock Maglev::PERSISTENT_ROOT
  #       rescue LockError
  #         puts "Got a lock error; sleep a while"
  #         sleep 1
  #         redo
  #       end
  #     end
  #
  # === Not Yet Supported
  #
  # The underlying GemStone system supports many lock related methods, not
  # all of them have been exposed to MagLev.  Among these features are:
  # * Efficient locking of collections
  # * Upgrading locks
  # * Release Lock Sets (see "Releasing Locks Upon Aborting or Committing"
  #   in the GemStone Programming Manual).
  # * Inquiring about locks and lock owners.
  # * Application Write Locks
  #
  class System

    # Transaction support
    class_primitive '__commitTransaction', 'commitTransaction'
    class_primitive '__abortTransaction', 'abortTransaction'
    class_primitive '__beginTransaction', 'beginTransaction'
    class_primitive '__beginNestedTransaction', 'beginNestedTransaction'
    class_primitive '__transactionLevel', 'transactionLevel'

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

    # Returns true if there are modified object in transient mode that have
    # not been commited to the repository.
    class_primitive 'needs_commit', 'needsCommit'

    # Enter a new nested transaction.
    # If session is outside of a transaction, equivalent to beginTransaction.
    # Signals a ImproperOperation exception if the begin would exceed
    # 16 levels of nested transactions.
    def self.begin_nested_transaction
      __beginNestedTransaction
    end

    # Returns 0 if not in a transaction, or a SmallInteger > 0
    # indicating the transaction level. > 1 means a nested
    # transaction.
    def self.transaction_level
      __transactionLevel
    end

    # Attempt to commit the transaction for the current session.
    #
    # This method is the same as 'commit_transaction' except for the
    # handling of locks.  If the commit succeeds, this method releases all
    # locks for the session and returns true.  Otherwise, it returns false
    # and does not release locks.
    #
    # This method also clears the commit release locks and commit-or-abort
    # release locks sets.  See the 'Releasing Locks' method category for
    # more information.
    #
    # Returns true if commit was read-only or succeeded , false if there
    # was a failure.
    class_primitive 'commit_and_release_locks', 'commitAndReleaseLocks'

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

    # call-seq:
    #   Maglev::System.read_lock an_object
    #
    # This method grants a read lock on <tt>an_object</tt> if no other
    # session holds a lock on an_object, or another session already holds a
    # read lock, but grants no lock if another session already holds a
    # write lock to anObject.  Returns Maglev::System
    #
    # Raises a LockError exception if another session holds a conflicting
    # lock.
    class_primitive 'read_lock', 'readLock:'

    # This method grants a write lock on <tt>an_object</tt> if no other
    # session holds a lock on an_object, or another session already holds a
    # read lock, but grants no lock if another session already holds a
    # write lock to anObject. Returns Maglev::System
    #
    # Raises a LockError exception if another session holds a conflicting
    # lock.
    class_primitive 'write_lock', 'writeLock:'

    # Removes the lock held by the current session on anObject. Returns the
    # receiver.  (see also #commit_and_release_locks).
    class_primitive 'remove_lock', 'removeLock:'

    # Removes all locks held by this session.  Returns the receiver. This
    # method succeeds even if the session no longer has read authorization
    # for one or more of its locked objects.
    class_primitive 'remove_locks_for_session', 'removeLocksForSession'

    # Removes all locks held by the current session on the objects in
    # aCollection.  If an object in aCollection is not locked by the
    # current session, that object is ignored. Returns the receiver.
    #
    # class_primitive 'remove_lock_all', 'removeLockAll:'

    class_primitive '__gem_version', 'gemVersionAt:'
    def self.__gs_version
      self.__gem_version('gsVersion')
    end

    def self.__gs_width
      case self.__gem_version('cpuArchitecture')
      when 'x86-64' then '64'
      when 'i386'   then '32'
      else '??'
      end
    end

    def self.__gs_dynlibext
      case self.__gem_version('osName')
      when 'Darwin' then '.dylib'
      else '.so'
      end
    end

    # Get the name of a known dynamic library shipped with GemStone.
    # Currently this includes
    #   * the linked GCI library
    #   * the RPC GCI library
    #   * the SSL library (OpenSSL including libcrypto)
    #   * the YAML library (libpsych)
    #
    def self.gs_lib_name(library_symbol)
      library_name = case library_symbol
                     when :LINKGCI then 'libgcilnk'
                     when :RPCGCI  then 'libgcirpc'
                     when :SSL     then 'libssl'
                     when :YAML    then 'libpsych'
                     else raise "Library name must be known"
      end
      "$GEMSTONE/lib/#{library_name}-#{self.__gs_version}-#{self.__gs_width}#{self.__gs_dynlibext}"
    end
  end
end
