# This is a demo of how you might implement the standard ruby PStore
# library using MagLev persistence, rather than the filesystem and marshal.


load "smalltalk/System.rb"
load "smalltalk/UserProfile.rb"
load "smalltalk/SymbolDictionary.rb"

load "pstore.rb"

# GStore is a MagLev native re-implementation of the Ruby standard library
# +PStore+.  GStore implements a Gemstone/MagLev based persistence
# mechanism based on a Hash.  User code can store arbitrary graphs of Ruby
# objects into the GStore file by name (keys).  User code may later read
# values back from the data store or update as needed.
#
# GStore is transactional, which ensures that updates to the store either
# succeed or fail together.
#
# GStore supports multiple, independent GStore databases.  PStore relies on
# Ruby's underlying marshal framework to write the objects to disk.  MagLev
# persistence does not need to marshal, it writes the object directly to
# disk.  If a program depends on the marshal hooks being called, then it
# may not work with GStore.  A future version of GStore may address this
# issue.
#
# +GStore+ manages several "files".  These are really Ruby Hash objects
# that are stored in the MagLev persistent root +UserGlobals+.
# +UserGlobals+ is a standard part of the GemStone VM, and is available for
# use by both Ruby and Smalltalk.
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

class GStore < PStore
  # Initialize a new GStore object.  The name of the GStore object will be
  # +file+ If the named GStore "file" already exists, its contents are used
  # as the inital state of the store.  Otherwise, a new, empty Hash is
  # created.
  def initialize(file="")
    raise PStore::Error, "Commit failed!" if (!Smalltalk::System._st_commitTransaction)

    # Get the user program root for storing persistent data within the VM.
    # This is known to the GemStone VM as :UserGlobals.  It is a Smalltalk
    # Dictionary (hash).
    user_globals = Smalltalk::System._st_myUserProfile._st_objectNamed :UserGlobals

    do_in_transaction {
      # @all_data is the Ruby Hash that will store each of the "files"
      # managed by GStore.  Each GStore "file" will be a Hash stored in
      # @all_data, keyed by the "file name".  @all_data is kept in
      # UserGlobals, so it, and all it contains, will be transactionally
      # persisted automatically by MagLev.
      @all_data = user_globals._st_at_ifAbsent(:GStore_data, nil)
      @all_data = user_globals._st_at_put(:GStore_data, Hash.new) unless @all_data
      @all_data[file] = Hash.new unless @all_data.key?(file)
    }
    @transaction = false
    @filename = file
  end

  def transaction(read_only=false) # :yields:  gstore
    raise PStore::Error, "nested transaction" if @transaction
    begin
      @transaction = true
      @rdonly = read_only
      @abort = false
      Smalltalk::System._st_beginTransaction
      @table = @all_data[@filename]
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

  def do_in_transaction
    for i in (1..10)
      yield
      return if (Smalltalk::System._st_commitTransaction)
      Smalltalk::System._st_abortTransaction
    end
    raise PStore::Error, "Unable to commit transaction"
  end
end
