# The server manages many Databases, and provides information.

module MDB
  class Server
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

    private
    def self.initialize_db_root
      if Maglev::PERSISTENT_ROOT[MDB::Server].nil?
        Maglev::PERSISTENT_ROOT[MDB::Server] = Hash.new
      end
      @proot = Maglev::PERSISTENT_ROOT[MDB::Server]
    end

    initialize_db_root   # rely on caller to commit
  end
end
