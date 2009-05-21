# This is a transliteration of the standard ruby PStore library into
# MagLev, making use of MagLev persistence, rather than a file on the file
# system.  The API of GStore, and most of the implementation, is identical
# to PStore.  I.e., API-wise, it can be a drop in replacement for PStore.
#
# PStore relies on Ruby's underlying marshal framework to write the objects
# to disk.  MagLev persistence does not need to marshal, it writes the
# object directly to disk.  If a program depends on the marshal hooks being
# called, then it may not work with GStore.  A future version of GStore may
# address this issue.
#
# NOTE: You must have created the smalltalk FFI wrappers in order to run
# this (rake dev:stwrappers).

# TODO: Add a way to remove a "file" from GStore.

# Other contenders to think about:
#   * *dbm
#   * madeline
#   * YAML
#   * Tokyo Cabinet
#   * CouchDB

load "smalltalk/System.rb"
load "smalltalk/UserProfile.rb"
load "smalltalk/SymbolDictionary.rb"

# GStore is a MagLev native re-implementation of the Ruby standard library
# +PStore+.  GStore implements a Gemstone/MagLev based persistence
# mechanism based on a Hash.  User code can store arbitrary graphs of Ruby
# objects into the GStore file by name (keys).  User code may later read
# values back from the data store or update as needed.
#
# GStore is transactional, which ensures that updates to the store either
# succeed or fail together.
#
# GStore supports multiple, independent GStore databases.

class GStore
  # The Error thrown by all GStore methods.
  class Error < StandardError; end

  # Initialize a new GStore object.  The name of the GStore object will be
  # +file+ If the named GStore "file" already exists, its contents are used
  # as the inital state of the store.  Otherwise, a new, empty Hash is
  # created.
  def initialize(file="")
    raise GStore::Error, "Commit failed!" if (!Smalltalk::System._st_commitTransaction)

    user_globals = Smalltalk::System._st_myUserProfile._st_objectNamed :UserGlobals
    do_in_transaction {
      all_data = user_globals._st_at_ifAbsent(:GStore_data, nil)
      if (!all_data)
        all_data = user_globals._st_at_put(:GStore_data, Hash.new)
      end
      if (!all_data.key? file)
        all_data[file] = Hash.new
      end
    }
    @transaction = false
    @filename = file
  end

  # Return the value stored under the key +name+
  def [](name)
    in_transaction
    @table[name]
  end

  # Set the value stored at +name+ to +value+.  Must be in a write
  # transaction.
  def []=(name,value)
    in_transaction_wr()
    @table[name] = value
  end

  # Abort the current transaction
  def abort()
    in_transaction
    abort = true
    throw :gstore_abort_transaction
  end

  # Commit the current transaction
  def commit()
    in_transaction
    abort = false
    throw :gstore_abort_transaction
  end

  # Remove the named element from the store.
  def delete(name)
    in_transaction_wr()
    @table.delete name
  end

  # Return the value stored at +name+.  If there is no key for +name+, then
  # return +default+.  If +default+ is not given, then raise and error if
  # no entry for +name+ has been stored.
  def fetch(name, default=GStore::Error)
    in_transaction
    unless @table.key? name
      if default==GStore::Error
        raise GStore::Error, format("undefined root name `%s'", name)
      else
        return default
      end
    end
    @table[name]
  end

  # Return the name of this GStore
  def path()
    @filename
  end

  def root?(name)
    in_transaction
    @table.key? name
  end

  def roots?
    in_transaction
    @table.keys
  end

  def transaction(read_only=false) # :yields:  gstore
    raise GStore::Error, "nested transaction" if @transaction
    begin
      @transaction = true
      @rdonly = read_only
      @abort = false
      Smalltalk::System._st_beginTransaction
      user_globals = Smalltalk::System._st_myUserProfile._st_objectNamed :UserGlobals
      all_data = user_globals._st_at :GStore_data
      @table = all_data[@filename]
      begin
        catch(:gstore_abort_transaction) do
          yield(self)
        end
      rescue Exception
        @abort = true
        raise
      ensure
        if read_only or @abort
          Smalltalk::System._st_abortTransaction
        else
          Smalltalk::System._st_commitTransaction
        end
      end
    ensure
      @transaction = false
      @table = nil
    end
  end

  private

  def in_transaction
    raise GStore::Error, "Must be in transaction" unless @transaction
  end

  def in_transaction_wr
    in_transaction
    raise GStore::Error, "Must be in write transaction" if @rdonly
  end

  def do_in_transaction
    for i in (1..10)
      yield
      return if (Smalltalk::System._st_commitTransaction)
      Smalltalk::System._st_abortTransaction
    end
    raise GStore::Error, "Unable to commit transaction"
  end
end
