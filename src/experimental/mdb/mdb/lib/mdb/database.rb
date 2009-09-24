require 'mdb/common.rb'

module MDB
  # A Database is a collection of documents, plus views that can be run
  # against the documents.
  class Database

    # Raised on an attempt to add a document to a key that already exists.
#    class DocumentAlreadyExists < MDB::MDBException ;  end

    class NoViewError < MDB::MDBError ;  end

    def initialize(name, view_class)
      set_view(view_class)
      @name = name
    end

    def set_view(view_class)
      @view_class = view_class
    end

    def execute_view(view_name, *params)
      view_sym = view_name.to_sym
      begin
        @view_class.send view_sym
      rescue NoMethodError
        raise NoViewError.new(view_name)
      end
    end
  end
end
