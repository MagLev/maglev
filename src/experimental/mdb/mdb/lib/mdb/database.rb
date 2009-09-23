require 'mdb/common.rb'

module MDB
  # A Database is a collection of documents, plus views that can be run
  # against the documents.
  class Database

    # Raised on an attempt to add a document to a key that already exists.
    class DocumentAlreadyExists < MDB::MDBException ;  end

    def initialize
      @documents = IdentitySet.new
    end

    def add_document(key, document)

    end
  end
end
