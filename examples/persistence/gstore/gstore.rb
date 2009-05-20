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

load "smalltalk/System.rb"
load "smalltalk/UserProfile.rb"
load "smalltalk/SymbolDictionary.rb"

class GStore
  class Error < StandardError; end

  def initialize(file="")
    GStore::Error.raise "Commit failed!" if (!Smalltalk::System._st_commitTransaction)

    userGlobals = Smalltalk::System._st_myUserProfile._st_objectNamed :UserGlobals
    allData = nil
    doInTransaction {
      allData = userGlobals._st_at_ifAbsent(:GStore_data, nil)
      if (!allData)
        allData = userGlobals._st_at_put(:GStore_data, Hash.new)
      end
      if (!allData.key? file)
        allData[file] = Hash.new
      end
    }
    @transaction = false
    @filename = file
  end

  def [](name)
    in_transaction
    @table[name]
  end

  def []=(name,value)
    in_transaction_wr()
    @table[name] = value
  end

  def abort()
    in_transaction
    abort = true
    throw :gstore_abort_transaction
  end

  def commit()
    in_transaction
    abort = false
    throw :gstore_abort_transaction
  end

  def delete(name)
    in_transaction_wr()
    @table.delete name
  end

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
      userGlobals = Smalltalk::System._st_myUserProfile._st_objectNamed :UserGlobals
      allData = userGlobals._st_at :GStore_data
      @table = allData[@filename]
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
    Error.raise "Must be in transaction" if !@transaction
  end

  def in_transaction_wr
    in_transaction
    Error.raise "Must be in write transaction" if @rdonly
  end

  def doInTransaction
    for i in (1..10)
      yield
      if (Smalltalk::System._st_commitTransaction)
        return
      end
      Smalltalk::System._st_abortTransaction
    end
    Error.raise "Unable to commit transaction"
  end
end
