module Maglev

  # +Model+ implements a very simple, and un-production-worthy, persistence
  # model.
  #
  # It provides the following services:
  # * Manages a class specific collection for instances of that class.
  # * Defines +persistent_new+ to create an instance and put it in the
  #   persistent collection.
  #
  # Some gotchas:
  # * If the module is re-included (or the file that defines the class is
  #   re-run), then all previous saved instances will be lost.
  module Model

    # These methods will be defined as class methods of the class that
    # includes this module.
    module ClassMethods

      # Creates a new instance and stages the new instance.  Equivalent to:
      #   instance = new(*args)
      #   stage instance
      def persistent_new(*args)
        instance = new(*args)
        stage instance
        instance
      end

      # Returns an array of all the instances for this class.
      def all
        Maglev::PERSISTENT_ROOT[self].values
      end

      # Get an object by object id.  If +id+ is not the id for an instance of
      # the calling class, then no object will be found (even if +id+ is the
      # valid id of soome instance of another class).
      def get(id)
        Maglev::PERSISTENT_ROOT[self][id.to_i]
      end

      # Stages +obj+ in the collection of persisted objects for this class.
      # This method does not commit, so the object will not be committed
      # until client code calls <tt>Maglev#commit_transaction</tt>.
      def stage(obj)
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
end
