# MDB::Model implements a very simple and un-production-worthy
# persistence model.  A class that includes the Maglev::Model module, will
# automatically persist all new instances in a hash table for the class.
# If the module is re-included (or the file that defines the class is
# re-run), then all previous saved instances will be lost.
module MDB::Model

  class NoRoot < MDB::Error;  end

  # These methods will be defined as class methods of the class that
  # includes this module.
  module ClassMethods

    # Set the
    def set_root(root)
      @root = root
    end

    def validate
      raise NoRoot if @root.nil?
    end

    # Create a new instance, and add it to the hash of persisted instances
    # for this class.
    def new(*params)
      validate
      obj = allocate
      obj.initialize(*params)
      add(obj)  # code smell
    end

    # Returns an array of all the posts
    def all
      validate
      Maglev::PERSISTENT_ROOT[self].values
    end

    # Get an object by object id.  If +id+ is not the id for an instance of
    # the calling class, then no object will be found (even if +id+ is the
    # valid id of soome instance of another class).
    def get(id)
      validate
      Maglev::PERSISTENT_ROOT[self][id.to_i]
    end

    # Add +obj+ to the set of persisted objects for this class.
    # called by new()
    def add(obj)
      validate
      Maglev::PERSISTENT_ROOT[self][obj.__id__] = obj
    end

    # Iterate over all saved items
    def each
      all.each do |el|
        yield el
      end
    end
  end

  def self.included(host)
    # TODO: Almost certainly not a good thing beyond the dev env: This will
    # blow away any persistent data from previous runs.  Perhaps we need a
    # migration idiom so that this can be controlled.
    Maglev::PERSISTENT_ROOT[host] = Hash.new
    host.extend ClassMethods
    host.extend Enumerable
  end

end
