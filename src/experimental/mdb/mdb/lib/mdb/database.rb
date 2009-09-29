require 'mdb/common.rb'

module MDB
  # A Database is a collection of documents, plus views that can be run
  # against the documents.
  #
  # == Transaction Management
  #
  # For phase one of MDB, we will only have the mdb_server.rb as a direct
  # client, or the client will be test scripts.  We will rely on the caller
  # to manage the transactions. MDB::ServerApp will wrap all http requests
  # in a transaction.
  #
  class Database
    # Raised on an attempt to add a document to a key that already exists.
    class DocumentAlreadyExists < MDB::MDBException ;  end

    class NoViewError < MDB::MDBError ;  end

    # Initialize a new Database. The database will remember its name, and
    # it will use the +view_class+ for all of its view requests.
    def initialize(name, view_class)
      set_view(view_class)
      @name = name
      @documents = Hash.new
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

    # Returns the document, or nil if the document does not exist.
    # Calls to_i on the document.
    def get(id)
      @documents[id.to_i]
    end

    # Add the document to the persistent set of documents this database
    # manages.  If the view class responds to :document_added, then call
    # <tt>document_added(doc_id, document)</tt> on the view.  Returns the
    # new document id.
    def add(document)
      # TODO: Need to validate that it is not already in there?
      doc_id = Gemstone.increment_pcounter(MDB::SHARED_COUNTER)
      raise DocumentAlreadyExists if @documents.key? doc_id # Shouldn't happen...
      @documents[doc_id] = document
      # We do the callback within the txn so that the model can
      # persistently update data structures and commit them.
      @view.document_added(doc_id, document) if @view.respond_to? :document_added
      doc_id
    end

    # Return the number of documents stored in the database
    def size
      @documents.size
    end

    #############################################
    # DEBUG METHODS:
    # TODO: remove debug methods
    #############################################
    def list_ids
      @documents.keys
    end

    def clear
      @documents.clear
    end
  end
end
