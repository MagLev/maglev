require 'mdb/common.rb'

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
      @view = view_class
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
      Maglev.abort_transaction
      # TODO: Need to validate that it is not already in there?
      @documents.add(document)
      @view.document_added(document) if @view.respond_to? :document_added
      Maglev.commit_transaction
      document.object_id  # TODO: Oop isn't proper doc id...
    end

    # Return the number of documents stored in the database
    def size
      @documents.size
    end
  end
end
