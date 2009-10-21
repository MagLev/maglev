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

    class ViewArgumentError < MDB::MDBError ;  end

    attr_reader :name

    # Initialize a new Database. The database will remember its name, and
    # it will use the +view_class+ for all of its view requests.
    def initialize(name, view_class)
      set_view(view_class)
      @name = name
      @documents = Hash.new
    end

    def debug_info
      result = "==== Database #{name}: Debug Info:\n"
      result << "\tview_class: #{@view.inspect}\n"
      view_methods = class << @view
                       self.instance_methods(false).inject("") { |acc,m| acc << " #{m.to_s}" }
                     end
      result << "\tview methods: #{view_methods}\n"
      result << "\tdocument count: #{size}\n"
      result
    end

    # Called by the server if the view class is updated (e.g., methods
    # added).
    def set_view(view_class)
      @view = validate_class(view_class)
    end

    def execute_view(view_name, *params)
      view_sym = view_name.to_sym
      args = [@documents] + params[0..-1]
      begin
        @view.send view_sym, *args
      rescue Exception => e
        raise NoViewError.new  <<-EOS
           DB #{@name}: Unable to execute view
           Exception: #{e.class} #{e.message}:
           Trying to run #{view_sym} on view class: #{@view} with args #{args.inspect}
           EOS
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

      # set the id field, if it has one
      document.id = doc_id if document.respond_to? :id=

      # We do the callback within the txn so that the model can
      # persistently update data structures and commit them.
      @view.document_added(doc_id, document) if @view.respond_to? :document_added
      doc_id
    end

    # Return the number of documents stored in the database
    def size
      @documents.size
    end

    # Returns the class for view_class, or raises an MDBError.  view_class
    # may be a class, or a string or a symbol representing the class.  This
    # will try to walk the package hierarchy looking for the class.
    def validate_class(view_class)
      return view_class if view_class.class == Class
      return false if view_class.nil?
      begin
        str = view_class.to_s
        klass = Object
        view_class.to_s.split('::').each do |prefix|
          klass = klass.const_get(prefix.to_sym)  # will raise name error
        end
        klass
      rescue NameError
        raise MDBError.new("Bad class: #{view_class.inspect}")
      end
    end
    private :validate_class

    def each
      @documents.each { |d| yield d }
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
