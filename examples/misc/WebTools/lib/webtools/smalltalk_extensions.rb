# Extra Smalltalk features used by WebTools

class Maglev::System
    class_primitive_nobridge 'stone_version_report', 'stoneVersionReport'
    class_primitive_nobridge 'gem_version_report',   'gemVersionReport'

    # Returns an Array of Fixnums corresponding to all of the sessions
    # currently running on the GemStone system.
    #
    class_primitive_nobridge 'current_session_ids', 'currentSessions'

    # Return the cache process slot number (a SmallInteger) for the given
    # session ID.  The session must be connected to the same shared page
    # cache as the session invoking this method.
    #
    # A return of nil indicates the session could not be located.
    class_primitive_nobridge 'cache_slot_for_sessionid', 'cacheSlotForSessionId:'

    # Returns an Array describing the session as follows:
    #
    #  1.  The UserProfile of the session, or nil if the UserProfile is recently
    #      created and not visible from this session's transactional view,
    #      or the session is no longer active.
    #  2.  The process ID of the Gem process of the session (an Integer).
    #  3.  The hostname of the machine running the Gem process
    #      (a String, limited to 127 bytes).
    #  4.  Primitive number in which the Gem is executing, or 0 if it is not executing
    #      in a long primitive.
    #  5.  Time of the session's most recent beginTransaction, commitTransaction, or
    #      abortTransaction (from System timeGmt).
    #  6.  The session state (a SmallInteger).
    #  7.  A SmallInteger whose value is -1 if the session is in transactionless mode,
    #      0 if it is not in a transaction and 1 if it is in a transaction.
    #  8.  A Boolean whose value is true if the session is currently referencing the
    #      oldest commit record, and false if it is not.
    #  9.  The session's serial number (a SmallInteger).
    #  10. The session's sessionId (a SmallInteger).
    #  11. A String containing the ip address of host running the GCI process.
    #      If the GCI application is linked (using libgcilnk*.so or gcilnk*.dll)
    #      this ip address is the address of the machine running the gem process .
    #  12. The priority of the session (a SmallInteger).
    #  13. Unique host ID of the host where the session is running (an Integer)
    #  14. Time of the session's most recent request to stone (from System timeGmt)
    #  15. Time the session logged in (from System timeGmt)
    #  16. Number of commits which have occurred since the session obtained its view.
    #
    #  Because a session can update its commit record without committing a
    #  transaction, it is possible that no session actually references the oldest
    #  commit record.  Therefore, the eighth element may be false for all current
    #  sessions.
    #
    #  To execute this method for any session other than your current session, you
    #  must have the SessionAccess privilege.
    #
    class_primitive_nobridge 'description_of_session', 'descriptionOfSession:'
end

# The stoneVersionReport returns a StringKeyValueDictionary.
# Unfortunately, that is on a different branch of the Smalltalk class
# hierarchy than RubyHash, so we need to add a few methods to let us
# iterate over the dictionary.
StringKeyValueDictionary = __resolve_smalltalk_global(:StringKeyValueDictionary)
class StringKeyValueDictionary
  primitive_nobridge 'keys', 'keys'
  primitive_nobridge 'at', 'at:'
end

