require 'mdb/common.rb'

# TODO: Need to review txn/locking stgy.  Are transactions per thread, or
# do we need to lock the db for each commit?

module MDB
  # A Database is a collection of documents, plus views that can be run
  # against the documents.
  class Database

    # Raised on an attempt to add a document to a key that already exists.
#    class DocumentAlreadyExists < MDB::MDBException ;  end

    class NoViewError < MDB::MDBError ;  end

    # Initialize a new Database. The database will remember its name, and
    # it will use the +view_class+ for all of its view requests.
    def initialize(name, view_class)
      set_view(view_class)
      @name = name
      @documents = IdentitySet.new
    end

    # Called by the server if the view class is updated (e.g., methods
    # added).
    def set_view(view_class)
      Maglev.transaction { @view = view_class }
    end

    def execute_view(view_name, *params)
      view_sym = view_name.to_sym
      begin
        @view.send view_sym
      rescue NoMethodError
        raise NoViewError.new("Database #{@name}: no view named: #{view_name}")
      end
    end

    def get(id)
      @object = ObjectSpace._id2ref(id)
    end

    # Add the document to the persistent set of documents this database
    # manages.  If the view class responds to :document_added, then call
    # the document_added hook in the view.  Returns the new document id.
    def add(document)
      Maglev.transaction do
        # TODO: Need to validate that it is not already in there?
        @documents.add(document)
        # We do the callback within the txn so that the model can
        # persistently update data structures and commit them.
        @view.document_added(document) if @view.respond_to? :document_added
      end
      document.object_id  # TODO: Oop isn't proper doc id...
    end

    # Return the number of documents stored in the database
    def size;  @documents.size  end

    #############################################
    # DEBUG METHODS:
    # TODO: remove debug methods
    #############################################
    def list_ids
      # IdentitySet.map isn't working right now...
      ids = []
      @documents.each { |d| ids << d.object_id }
      ids
    end
    def clear;  @documents.clear  end
  end
end
