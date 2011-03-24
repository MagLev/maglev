# Extra Smalltalk features used by WebTools

class Maglev::System
    class_primitive_nobridge 'stone_version_report', 'stoneVersionReport'
    class_primitive_nobridge 'gem_version_report',   'gemVersionReport'

    # Returns an Array of Fixnums corresponding to all of the sessions
    # currently running on the GemStone system.
    #
    class_primitive_nobridge 'current_sessions', 'currentSessions'

    # Return the cache process slot number (a SmallInteger) for the given
    # session ID.  The session must be connected to the same shared page
    # cache as the session invoking this method.
    #
    # A return of nil indicates the session could not be located.
    class_primitive_nobridge 'cache_slot_for_sessionid', 'cacheSlotForSessionId:'

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

