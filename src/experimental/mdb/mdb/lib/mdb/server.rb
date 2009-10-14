# The server manages many Databases, and provides information.

# Be careful not to Maglev.abort_transaction in this code, as it may throw
# away stuff a direct maglev client has loaded.  E.g., if app code loads a
# new view class, and then tries to call Server.update within the same txn,
# and if Server.update did an abort...
#
# == TODO
# * ensure fresh views for readers.
# * make a real class rather than all class side methods.
#
module MDB
  class Server
    # Raised if there is a request to create a database, but one with that
    # name already exists
    class DatabaseExists < MDBError; end

    def initialize
      @dbs = Hash.new
    end

    def debug_info
      result = "Debug Information for MDB::Server:\n"
      result << "\t@dbs:      #{@dbs.inspect}\n"
      result << "\t@dbs.keys: #{@dbs.keys.inspect}\n"
      @dbs.each { |key, value| result << value.debug_info }
      result
    end

    # Create a new database.  Returns the new database.
    # Raises DatabaseExists, if db_name already exists.
    def create(db_name, view_class)
      ensure_not_nil(db_name,    'MDB::Server#create: db_name')
      ensure_not_nil(view_class, 'MDB::Server#create: view_class')
      key = db_name.to_sym
      raise DatabaseExists.new(key) if @dbs.has_key?(key)
      Maglev.transaction { @dbs[key] = Database.new(db_name, view_class) }
      @dbs[key]
    end

    # Updates a database .  Returns new database.
    # Raises DatabaseNotFound, if db_name does not exist.
    def update(db_name, view_class)
      db = get_db(db_name, "#{self}.update")
      db.set_view(view_class)
    end

    # Removes the Database named +db_name+ from the Server.
    # Raises DatabaseNotFound if there is no such database.
    # returns the database
    def delete(db_name)
      get_db(db_name, "#{self}.delete") # raises DatabaseNotFound if necessary
      Maglev.transaction { @dbs.delete db_name.to_sym }
    end

    # Returns the database named +db_name+, or nil.
    def [](db_name)
      @dbs[db_name.to_sym]
    end

    # Returns an array of the db names
    def db_names
      # TODO: This is throwing an exception right now...
      # @dbs.keys.inspect
      result = []
      @dbs.each { |k,v| result << k }
      result
    end

    def key?(db_name)
      @dbs.key? db_name.to_sym
    end

    class DatabaseNotFound < MDB::MDBError; end

    def self.initialize_db_root
      if Maglev::PERSISTENT_ROOT[MDB::Server].nil?
        Maglev::PERSISTENT_ROOT[MDB::Server] = MDB::Server.new
      end
    end

    def self.server
      Maglev::PERSISTENT_ROOT[MDB::Server]
    end

    private

    def get_db(db_name, msg)
      key = db_name.to_sym
      unless @dbs.has_key?(key)
        raise DatabaseNotFound.new(
          "#{msg}: db not found: #{db_name.inspect}")
      end
      @dbs[key]
    end

    def ensure_not_nil(param, msg)
      raise ArgumentError.new("#{msg}: nil") if param.nil?
    end

  end
end
