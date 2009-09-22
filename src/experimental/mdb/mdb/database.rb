# A Database holds a set of documents, views on those documents and manages
# indicies on the documents.  The Database class manages a persistent hash
# of the currently registered Database instances.
module Maglev
  def transaction(&block)
    Maglev.abort_transaction
    Maglev.persistent do
      block.call
    end
    Maglev.commit_transaction
  end
  module_function :transaction
end

module MDB
  # All MDB errors derive from MDBError
  class MDBError < RuntimeError; end

  class Database
    # Raised if there is a request to create a database, but one with that
    # name already exists
    class DatabaseExists < MDBError; end

    # Raised for any operation on a database, but the database is not
    # found.
    class DatabaseNotFound < MDBError; end

    def self.debug_info
      STDERR.puts "-- @proot: #{@proot.inspect}"
      @proot.each do |key, value|
        STDERR.puts "-- #{key.inspect} => #{value.inspect}"
      end
    end

    # Create a new database.  Returns the new database.
    # Raises DatabaseExists, if db_name already exists.
    def self.create(db_name)
      key = db_name.to_sym
      raise DatabaseExists.new(key) if @proot.has_key?(key)
      Maglev.transaction { @proot[key] = Database.new }
      @proot[key]
    end

    # Removes the Database named +db_name+ from the Server.
    # Raises DatabaseNotFound if there is no such database.
    def self.delete(db_name)
      key = db_name.to_sym
      raise DatabaseNotFound.new(key) unless @proot.has_key?(key)
      Maglev.transaction { @proot.delete key }
    end

    # Returns the database named +db_name+, or nil.
    def self.[](db_name)
      @proot[db_name.to_sym]
    end

    # Returns an array of the db names
    def self.db_names
      @proot.keys
    end

    # holding on to a view class may be problematic, as each time we update
    # the view class (with a new view)
    def initialize
      @views     = nil
      @documents = IdentitySet.new
    end

    def run_view(view_name)
      @views.send(view_name.to_sym) unless @views.nil?
    end

    private
    def self.initialize_db_root
      if Maglev::PERSISTENT_ROOT[MDB::Database].nil?
        Maglev::PERSISTENT_ROOT[MDB::Database] = Hash.new
      end
      @proot = Maglev::PERSISTENT_ROOT[MDB::Database]
    end

    initialize_db_root   # rely on caller to commit
  end
end

