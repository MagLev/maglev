# Reduced Conflict Hash
# RCHash is identically Smalltalk RcKeyValueDictionary

RCHash = __resolve_smalltalk_global(:RcKeyValueDictionary)

# RCHash stores key/value pairs, but does so in a manner designed to reduce
# the chances of a concurrency conflict when multiple VMs modify an
# instance.
#
# RCHash has an important restriction: after a key/value pair has been
# added to an RCHash, you must not modify the key.  Doing so renders the
# value inaccessible.
#
# Unlike Hash, RCHash provides for concurrent handling of an individual
# instance by multiple sessions.  Any or all of those sessions can modify
# the single instance.  When that happens, RCHash reduces (but does not
# eliminate) the transaction conflicts that can arise among those sessions
# when they attempt to commit the instance to GemStone.
#
# Commit Conflicts.
#
# In general, +RCHashes+ do not cause concurrency conflicts for write
# operations that are commutative (operations that can be performed in any
# order without affecting the final GemStone state).  However, under some
# circumstances a user may experience conflict for commutative operations
# when the size of the hash is too small (relative to the number of write
# operations performed in a transaction).  This can be avoided by creating
# a larger hash with the +new+ method, or increasing an existing hash's
# size with the <tt>rebuild_table</tt> method.
#
# If multiple users change values for different keys in a single RCHash,
# the changes do not usually cause conflicts at commit time.  However,
# there is a (narrow and uncommon) window of time over which users have no
# control during which such a set of changes could result in conflicts.
#
# An RCHash is an equality-based collection.  That is, two keys or two
# values are considered to be the same if they are equivalent; they need
# not be identical to be the same.  Thus, if you add two key-value pairs to
# an RCHash but the keys are equivalent, even if they are not identical,
# then the result is that the second pair overwrites the first one, because
# the keys are the same.

class RCHash
  include Enumerable

  class_primitive 'new', 'new'

  primitive '[]=', 'at:put:'
  primitive '__at_otherwise',  'at:otherwise:'
  primitive 'size', 'size'
  alias length size
  alias count size
  primitive '__delete&', 'removeKey:ifAbsent:'
  primitive 'rebuild_table', 'rebuildTable:'
  primitive 'empty?', 'isEmpty'
  primitive '__keys', 'keys'
  primitive '__values&', 'valuesDo:'
  primitive 'each&', 'keysAndValuesDo:'

  def keys
    __keys.to_a
  end

  def values
    ary = []
    __values do |v|
      ary << v
    end
    ary
  end

  def [](key)
    __at_otherwise(key, nil)
  end

  def delete(key)
    __delete(key) { return nil }
  end
end

