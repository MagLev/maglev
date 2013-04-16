# This is a demo of how you might implement the standard ruby PStore
# library using MagLev persistence, rather than the filesystem and marshal.

load "pstore.rb"

# +GStore+ is a MagLev native re-implementation of the Ruby standard
# library +PStore+.  +GStore+ implements a Gemstone/MagLev based
# persistence mechanism based on a Hash.  User code can store
# arbitrary graphs of Ruby objects into the +GStore+ file by name
# (keys).  User code may later read values back from the data store or
# update as needed.
#
# +GStore+ uses a nested transaction. To actually commit across
# process and to the stone, user code will have to call
# +Maglev.commit_transaction+ outside a +GStore+ transaction. This is
# to ensure that using +GStore+ doesn't mess with any user code using
# the Maglev persistence.
#
# +GStore+ supports multiple, independent +GStore+ databases.  PStore
# relies on Ruby's underlying marshal framework to write the objects
# to disk.  MagLev persistence does not need to marshal, it writes the
# object directly to disk. If a program depends on the marshal hooks
# being called, then it may not work with +GStore+.  A future version
# of +GStore+ may address this issue.
#
# +GStore+ manages several "files".  These are really Ruby Hash
# objects that are stored in the MagLev persistent root
# +Maglev::PERSISTENT_ROOT+.  +Maglev::PERSISTENT_ROOT+ is stored in
# the standard GemStone VM variable +UserGlobals+, and is available
# for use by both Ruby and Smalltalk.
#

class GStore < PStore
  # Initialize a new +GStore+ object.  The name of the +GStore+ object will be
  # +file+ If the named +GStore+ "file" already exists, its contents are used
  # as the inital state of the store.  Otherwise, a new, empty Hash is
  # created.
  def initialize(file="")
    # @all_data is the Ruby Hash that will store each of the "files"
    # managed by +GStore+.  Each +GStore+ "file" will be a Hash stored in
    # @all_data, keyed by the "file name".  @all_data is kept in
    # +Maglev::PERSISTENT_ROOT+, so it, and all it contains, will be
    # transactionally persisted automatically by MagLev.
    @all_data = GStore.get_all_data
    GStore.do_in_transaction do
      @all_data[file] = Hash.new unless @all_data.key?(file)
    end

    @transaction = false
    @filename = file
  end

  def transaction(read_only=false) # :yields:  gstore
    raise PStore::Error, "nested transaction" if @transaction
    begin
      @transaction = true
      @rdonly = read_only
      @abort = false
      Maglev.begin_nested_transaction
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
          Maglev.abort_transaction
        else
          Maglev.commit_transaction
        end
      end
    ensure
      @transaction = false
      @table = nil
    end
  end

  # Remove the +GStore+ "file" from the persistent store.  This is analagous
  # to deleting the PStore db file (rm pstore.db).
  def self.rm(file)
    all_data = self.get_all_data
    GStore.do_in_transaction { all_data.delete(file) }
  end

  private

  def self.get_all_data
    GStore.do_in_transaction {
      Maglev::PERSISTENT_ROOT[:all_data] ||= Hash.new
    }
  end

  def self.do_in_transaction
    for i in (1..10)
      v = yield
      return v if (Maglev.commit_transaction)
      Maglev.abort_transaction
    end
    raise PStore::Error, "Unable to commit transaction"
  end
end
